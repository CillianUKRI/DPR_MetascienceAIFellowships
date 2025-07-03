library(tidyverse)
library(here)
library(DT)
library(psych) # to calculate ICCs
library(irr) # to calculate ICCs
library(lme4)

set.seed(122)

# Load in score data ------------------------------------------------------

# this data file cannot be shared but consists of 2 columns
# the format looks like this, and has ~800 rows
# Application | Score
# <character> | <numeric>

realscores <- read_csv(here("inputdata", "DPRScoresDownloadFromTFS_Formatted050625.csv")) |>
  select(`Application ID`, Score) |>
  rename("Application" = `Application ID`) |>
  mutate(Application = paste0("APP", Application)) 

# Monday 9 June -----------------------------------------------------------

# Create data frame for values to go
diffICCs <- data.frame(numberreviewers = c(3,4,5,6,7,8,9),
                       meanICC = rep(NA,7),
                       medianICC = rep(NA,7),
                       ICClowCI = rep(NA,7),
                       ICChighCI = rep(NA,7),
                       SEMmean = rep(NA,7),
                       SEMmedian = rep(NA,7),
                       SEMlowCI = rep(NA,7),
                       SEMhighCI = rep(NA,7),
                       SDDmean = rep(NA,7),
                       SDDmedian = rep(NA,7),
                       SDDlowCI = rep(NA,7),
                       SDDhighCI = rep(NA,7))

# Convert the longform data to wide
long_data <- realscores %>%
  group_by(Application) |>
  slice_head(n=9) |>
  filter(n() == 9) |> # Keep only 9 reviews from each Application (and remove those that have less, to remove NA values)
  mutate(reviewer_id = paste0("Reviewer", row_number())) |>
  ungroup()


# Group by application and nest scores
# This creates a table with 9 columns and 80 rows. A column for each reviewer, row for each application
full_icc_data <- long_data %>%
  group_by(Application) %>%
  slice_head(n=9) |>
  summarise(scores = list(Score)) %>%
  pull(scores) %>%
  do.call(rbind, .)


# Calculate the mean score for each application
mean_scores <- rowMeans(full_icc_data, na.rm = TRUE)


# Calculate the standard deviation of the mean scores
sd_scores <- sd(mean_scores)

# Calculate ICC using one-way random effects model
icc_fullnine <- icc(full_icc_data, model = "oneway", type = "agreement", unit = "average")
summary(icc_fullnine)
print(icc_fullnine)


# Then compute SEM and SDD
sem_nine <- sd_scores * sqrt(1 - icc_fullnine$value)
sdd_nine <- 1.96 * sqrt(2) * sem_nine

diffICCs$SDDmean[7] <- sdd_nine   
diffICCs$meanICC[7] <- icc_fullnine$value               

# ---- Simulate ICC with 3 Reviewers per Application ----

n_sim <- 10000
icc_values <- numeric(n_sim)
sd_values <- numeric(n_sim)
sem_values <- numeric(n_sim)
sdd_values <- numeric(n_sim)

for(reviewers in 3:8) {
  icc_values <- numeric(n_sim)
  sd_values <- numeric(n_sim)
  sem_values <- numeric(n_sim)
  sdd_values <- numeric(n_sim)
  for (i in 1:n_sim) {
    numberreviewers <- reviewers
    
    sampled_reviews <- long_data %>%
      group_by(Application) %>%
      sample_n(numberreviewers) %>%
      ungroup()
    
    sim_icc_data <- sampled_reviews %>%
      group_by(Application) %>%
      summarise(scores = list(Score)) %>%
      pull(scores) %>%
      do.call(rbind, .)
    
    if (all(complete.cases(sim_icc_data))) {
      icc_sim <- icc(sim_icc_data, model = "oneway", type = "agreement", unit = "average")
      icc_values[i] <- icc_sim$value
      sd_simicc <- sd(rowMeans(sim_icc_data, na.rm=T))
      sem_values[i] <- sd_simicc * sqrt(1-icc_values[i])
      sdd_values[i] <- 1.96*sqrt(2) * sem_values[i]
      

    } else {
      icc_values[i] <- NA
    }
  }
  meanICC <- mean(icc_values)
  medianICC <- median(icc_values)
  meanSEM <- mean(sem_values)
  medianSEM <- median(sem_values)
  meanSDD <- mean(sdd_values)
  medianSDD <- median(sdd_values)
  
  diffICCs$meanICC[numberreviewers-2] <- meanICC
  diffICCs$medianICC[numberreviewers-2] <- medianICC
  diffICCs$ICClowCI[numberreviewers-2] <- quantile(icc_values, probs = c(0.025, 0.975))[[1]]
  diffICCs$ICChighCI[numberreviewers-2] <- quantile(icc_values, probs = c(0.025, 0.975))[[2]]
  
  diffICCs$SEMmean[numberreviewers-2] <- meanSEM
  diffICCs$SEMmedian[numberreviewers-2] <- medianSEM
  diffICCs$SEMlowCI[numberreviewers-2] <- quantile(sem_values, probs = c(0.025, 0.975))[[1]]
  diffICCs$SEMhighCI[numberreviewers-2] <- quantile(sem_values, probs = c(0.025, 0.975))[[2]]
  
  diffICCs$SDDmean[numberreviewers-2] <- meanSDD
  diffICCs$SDDmedian[numberreviewers-2] <- medianSDD
  diffICCs$SDDlowCI[numberreviewers-2] <- quantile(sdd_values, probs = c(0.025, 0.975))[[1]]
  diffICCs$SDDhighCI[numberreviewers-2] <- quantile(sdd_values, probs = c(0.025, 0.975))[[2]]
  
  print(paste0("Finished simulating ", reviewers, " reviewer(s)."))
}

write_csv(diffICCs, "outputs/ICC_SDDs_used_for_AYIM110625.csv")

# Plot fordiffICCs# Plot for SDD

diffICCs$lowerrorbars <- diffICCs$SDDmean - diffICCs$SDDlowCI
diffICCs$higherrorbars <- diffICCs$SDDhighCI - diffICCs$SDDmean

diffICCs$ICClowerrorbars <- diffICCs$meanICC - diffICCs$ICClowCI
diffICCs$ICChigherrorbars <- diffICCs$ICChighCI - diffICCs$meanICC

diffICCs$source <- c(rep("Bootstrapped", length(diffICCs$numberreviewers)-1), "Real")


SDDplot <- ggplot(diffICCs, aes(x = numberreviewers, y = SDDmean, color=source, shape=source)) +
  geom_point(size=3) +
  geom_line(aes(group=1), color="grey40") +
  geom_errorbar(aes(ymin = SDDmean - lowerrorbars, ymax = SDDmean + higherrorbars), width = 0.2, na.rm = TRUE) +
  scale_color_manual(values = c("Bootstrapped" = "steelblue", "Real" = "firebrick"), name=NULL) +
  scale_shape_manual(values = c("Bootstrapped" = 16, "Real" = 17), name=NULL) +
  scale_x_continuous(breaks=2:9) +
  labs(
    title = "",
    x = "Number of Reviewers",
    y = "Mean SDD",
    color = "Data Source",
    shape = "Data Source"
    
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.95, 0.7), # x = 95% from left, y = 10% from bottom
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(fill = "white", color = NA)
  )

ggsave("outputs/SDDplot.png", plot = SDDplot, width = 10, height = 6, dpi = 300)

SDDplot

ICCplot <- ggplot(diffICCs, aes(x = numberreviewers, y = meanICC, color=source, shape=source)) +
  geom_point(size=3) +
  geom_line(aes(group=1), color="grey40") +
  geom_errorbar(aes(ymin = meanICC - ICClowerrorbars, ymax = meanICC + ICChigherrorbars), width = 0.2, na.rm = TRUE) +
  scale_color_manual(values = c("Bootstrapped" = "steelblue", "Real" = "firebrick"), name=NULL) +
  scale_shape_manual(values = c("Bootstrapped" = 16, "Real" = 17), name=NULL) +
  scale_x_continuous(breaks=2:9) +
  labs(
    title = "",
    x = "Number of Reviewers",
    y = "Mean ICC",
    color = "Data Source",
    shape = "Data Source"
    
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.95, 0.1), # x = 95% from left, y = 10% from bottom
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(fill = "white", color = NA)
  )

ggsave("outputs/ICCplot.png", plot = ICCplot , width = 10, height = 6, dpi = 300)

ICCplot


# Summary and plot
summary(icc_values)


