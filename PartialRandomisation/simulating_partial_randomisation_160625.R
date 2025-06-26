
library(plyr)
library(ggplot2)
library(reshape2)
library(scales)
library(viridis)
library(plotly)
library(reshape2)
library(odbc)
library(DBI)


setwd("C:/Users/ahul01/OneDrive - UKRI/Desktop/R_projects/2025/simulating_partial_randomisation")

plot_width <- 8
plot_height <- 8
n_props <- 5e5			# number of simulated proposals to create in the first version of the simulation
meeting_count <- 1e5		# number of simulated meetings to generate in the first version of the simulation



##################################################################################################
##  Part 1 -    create a function that can generate 'proposals' with reviewer scores
##              in two situations: biased and unbiased

## parameters for beta distributions used to determine proposal fundability and reviewer scores need to be set manually
## the following ranges create a pretty reliable facsimile of real values, but you may wish to change them
##

min_shape1 <- 2
max_shape1 <- 20
min_shape2 <- 4
max_shape2 <- 7

##
## to calculate the values of shape 1 (alpha) and shape2 (beta) needed so that
## the mean remains the same in both types of review, use these relationships from:
## https://stats.stackexchange.com/questions/12232/calculating-the-parameters-of-a-beta-distribution-using-the-mean-and-variance
##
##    shape1_highvar <- ((1 - fundability) / high_variance - 1 / fundability) * fundability ^ 2
##    shape2_highvar <- shape1_highvar * (1 / fundability - 1)
##
## argument 'bias' is the underlying baised score as a % of the unbiased score
## argument 'group' is which group of scores to return (biased or unbiased)
## the output is a df with a column indicating whether the scores relate to biased or an unbiased score for that proposal, and the relevant scores
## determine values to split continuous [0,1] range into discrete 1 to 6 scores

score_breaks <- c(-Inf, 1/6, 1/3, 0.5, 2/3, 5/6, Inf)
score_labels <- c(1,2,3,4,5,6)

## the function itself

score_generator <- function(min_shape1, max_shape1, min_shape2, max_shape2, max_reviews, bias) {
  
  ## randomly sample parameters in range determined
  
  shape1_val <- runif(n = 1, min = min_shape1, max = max_shape1)
  shape2_val <- runif(n = 1, min = min_shape2, max = max_shape2)
  
  ## calculate mean (which is the true underlying fundability score for the 'proposal', between 0 and 1) and variance of reviewer scores
  
  fundability <- shape1_val / (shape1_val + shape2_val)
  score_variance <- (shape1_val * shape2_val) / ( ((shape1_val + shape2_val) ^ 2) * (shape1_val + shape2_val + 1) )
  
  ## in the biased case
  ## the variance in fundability should stay the same, but the mean should decrease
  ## by an amount determined by 'bias'
  ## so we have a proposal that has the same variation in reviewer scores but with reduced apparent fundability
  
  biased_score <- fundability * bias
  shape1_biased <- ((1 - biased_score) / score_variance - 1 / biased_score) * biased_score ^ 2
  shape2_biased <- shape1_biased * (1 / biased_score - 1)
  
  ## now create a sample of reviewer scores in both the biased and unbiased cases
  ## and discretise the scores into the 1 to 6 scale used
  
  proposal_scores <- data.frame( 
    unbiased_scores = rbeta(n = max_reviews, shape1 = shape1_val, shape2 = shape2_val),
    biased_scores = rbeta(n = max_reviews, shape1 = shape1_biased, shape2 = shape2_biased)
  )
  
  proposal_scores$unbiased <- as.numeric(cut(proposal_scores$unbiased_scores, breaks = score_breaks,
                                             labels = score_labels ))
  proposal_scores$biased <- as.numeric(cut(proposal_scores$biased_scores, breaks = score_breaks,
                                           labels = score_labels ))
  proposal_scores <- suppressMessages(melt(proposal_scores[ , c("unbiased", "biased")]))
  names(proposal_scores) <- c("type", "scores")
  proposal_scores$actual_fundability <- fundability
  return(proposal_scores)
  
}


## Part 2 - create a reservoir of simulated proposals
## use the beta values above
## the number of reviews for each proposal is determined in the list generation
## no proposal can have fewer than 3 reviews
## bias is also determined in each iteration so that the whole landscape of biases can be covered (this could be limited by adapting the function)
## when run, some values of bias will lead to warnings as NAs are created.
## Affected proposals have their NA scores set to 1, as this is the lowest possible
## We only need the average score, true fundability and bias for a set of reviews, not the reviews themselves
## Also we need to identify proposals from groups that will and will not suffer from bias
## This will be used to determine success rate differences between hypothetical groups, A and B, in a later simulation


proposal_list <- vector(mode = "list", length = n_props)
proposal_list_full <- vector(mode = "list", length = n_props)	# to capture proposals' biased and unbiased scores, if needed (commented out usually)
biased_proposal_list <- vector(mode = "list", length = n_props)

for(i in 1:n_props) {
  
  if(i %% 100 == 0) message(paste0("Generating proposal ", i, " of ", n_props))
  
  max_reviews <- rpois(1, 3)			## keep the number of reviews at around 3
  if(max_reviews < 3) max_reviews <- 3	## but if it's less than 3, set as 3
  
  proposal_bias <- rbeta(1, 6, 1.5)		## determine the bias, from 0 to 1 (NB higher value = less bias against). Change these to suit.
  ## https://homepage.divms.uiowa.edu/~mbognar/applets/beta.html is helpful
  
  x <- score_generator(min_shape1, max_shape1, min_shape2, max_shape2, max_reviews, proposal_bias)
  x$proposal_bias <- proposal_bias
  x$scores[is.na(x$scores)] <- 1			## if the score would have been less than 1 due to undefined bias, set as 1
  
  sample_size <- rpois(1, 3)			## as before, we need about 3 reviews, and not fewer than 3
  if(sample_size < 3) {			## and not more than are there in the first place
    
    sample_size <- 3
    
  } else if(sample_size > nrow(x)) {
    
    sample_size <- nrow(x)
    
  }
  
  #proposal_list_full[[i]] <- x	# to capture both sets of scores, if desired - usually comment out
  
  # at this point the process separates into a part that returns a set of reviews which is a mix of biased and unbiased, 
  # and one in which we take either the unbiased or the biased reviews
  # these results being placed in the relevant list
  
  mix <- x[sample(1:nrow(x), size = sample_size), ]	# gives a mix of biased and unbiased reviews, with even probability
  
  determine_group <- runif(1)
  if(determine_group <= 0.5) {				# means that half the proposals are subject to bias, half are not
    biased <- x[which(x$type == "biased"),]
    applicant_type <- "subject_to_bias"
  } else {
    biased <- x[which(x$type == "unbiased"),]
    applicant_type <- "not_subject_to_bias"
  }
  
  # the proposals that are a mix of biased and unbiased reviews
  
  proposal_list[[i]] <- data.frame(
    
    proposal_number = i,
    mean_proposal_score = mean(mix$score),
    mean_proposal_bias = (sum(mix$proposal_bias[mix$type == "biased"]) + length(mix$proposal_bias[mix$type == "unbiased"]))/nrow(mix),
    mean_unbiased_proposal_score = mean(mix$score[mix$type == "unbiased"]),
    n_revs = nrow(mix),
    fundability = mean(x$actual_fundability)
    
  )
  
  # the proposals where the applicant either is or is not subject to bias
  
  biased <- data.frame(
    
    proposal_number = i,
    mean_proposal_score = mean(biased$score),
    mean_proposal_bias = unique(biased$proposal_bias),
    n_revs = nrow(biased),
    fundability = unique(biased$actual_fundability),
    applicant_type = applicant_type
    
  )
  
  biased$mean_proposal_bias <- ifelse(biased$applicant_type == "subject_to_bias", biased$mean_proposal_bias, 1)
  
  biased_proposal_list[[i]] <- biased
  
}


## convert lists to dataframes


proposals <- ldply(proposal_list, .id = NULL)
biased_proposals <- ldply(biased_proposal_list, .id = NULL)


## have a look at the distribution of mean scores for unbiased proposals
## to check that it's realistic
## NB - this part requires being connected to UKRI DataBank
## first get the real distribution of scores


score_query <- "SELECT 

        fapr.ApplicationID,
        fapr.DateFirstInviteIssue,
	fapr.AdministratingCouncil,
        fapr.Score
        
    FROM databank.fact_application_peer_review AS fapr

	WHERE
	fapr.DateFirstInviteIssue >= '2023-01-01'
	AND
	fapr.DateFirstInviteIssue < '2024-01-01'

    "



real_scores <- dbGetQuery(  conn = dbConnect(odbc(), "DataBank"),
                            statement = score_query)
real_scores_summary <- ddply(real_scores[which(real_scores$AdministratingCouncil != "Innovate UK"), ], c("ApplicationID"), summarise,
                             scores = length(ApplicationID), mean_score = mean(Score, na.rm = TRUE))
real_scores_summary <- real_scores_summary[which(real_scores_summary$scores > 2), ]
real_scores_summary <- real_scores_summary[complete.cases(real_scores_summary), ]


## plot both together

score_dist_sim <- proposals[ , c("proposal_number", "mean_unbiased_proposal_score")]
names(score_dist_sim) <- c("proposal_id", "mean_score")
score_dist_sim$ind <- "Simulated"

score_dist_real <- real_scores_summary[ , c("ApplicationID", "mean_score")]
names(score_dist_real) <- c("proposal_id", "mean_score")
score_dist_real$ind <- "Actual"

all_score_dists <- rbind(score_dist_sim[sample(1:nrow(score_dist_sim), nrow(score_dist_real)), ], score_dist_real)

all_score_distribution_plot <- ggplot(all_score_dists) +
  geom_density(aes(x = mean_score)) +
  facet_wrap( ~ ind, scales = "free_y") +
  labs(	x = "\nMean proposal score",
        y = ""
  ) +
  scale_x_continuous(breaks = c(0:6), limits = c(1,6)) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.2, colour = "grey66"),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_text(colour = "grey44", size = 14),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(colour = "grey66", size = 10),
    strip.text = element_text(size = 12, colour = "white", face = "bold"),
    plot.background = element_rect(fill = "#F3F2F1", colour = NA),
    strip.background = element_rect(fill = "black", colour = "black"),
    legend.text = element_text(colour = "grey44"),
    legend.title = element_text(colour = "grey44", vjust = 0.5)
  )

all_score_distribution_plot

ggsave("08_comparison_of_real_and_simulated_proposals.pdf", width = plot_width, height = plot_height)
ggsave("08_comparison_of_real_and_simulated_proposals.svg", width = plot_width, height = plot_height)


###########################################################################################################################################################
## Generating meetings using simulated proposals
## The general idea here is to sample proposals from the relevant df to generate simulated panel meetings which have the reliability of their decisions measured
## In each meeting there is a correct order, determined by the 'actual_fundability'
## And a meeting order, determined by the average reviewer score, with appropriate handling of ties
## For each proposal, sample a random number of biased and unbiased review scores to give the actual set of reviews
## For each meeting calculate the average bias level across all reviews, and the accuracy, F1 etc of the result
## Incorporate a specific partial randomisation scenario into each meeting - don't reuse the same meeting with different randomisations.
## This requires sampling a success rate for a meeting, and a % funded outright for each
## a reasonable definition of the success rate distribution is rbeta(6, 14) which gives an average
## rate of 30% and isn't too wide
## 'frac_funded_outright' is the % of funded proposals that are funded without randomisation
## this is a fixed value, probably around 0.5. It could be allowed to vary, if additional complexity is required.
## the function used to create a meeting
## note that the function defines its source as being 'proposals', which is the df created earlier. This uses a mix of biased and unbiased reviews


create_meeting <- function(number_of_proposals, rate_shape_1, rate_shape_2, frac_funded_outright) {
  
  meeting_size <- rpois(1, number_of_proposals)						# proposal sample size
  meeting_proposals <- proposals[sample(nrow(proposals), meeting_size), c("mean_proposal_score", "mean_proposal_bias", "fundability")]
  award_rate <- rbeta(1, rate_shape_1, rate_shape_2)					# determine meeting award rate
  n_funded <- round(award_rate * nrow(meeting_proposals), digits = 0)			# determine how many are funded overall
  n_funded_outright <- round(n_funded * frac_funded_outright, digits = 0)			# determine how many funded outright
  meeting_proposals <- meeting_proposals[order(-meeting_proposals$mean_proposal_score), ]	# order proposals by their mean reviewer score
  
  # to create a column with a decision process value (funded_outright, randomised, rejected_outright) for each application
  # first, name those funded outright
  
  funded_outright <- rep("funded_outright", times = n_funded_outright)		
  
  # then those randomised
  randomised <- rep("randomised", times = runif(1) * (nrow(meeting_proposals) - n_funded_outright))	# uniform random % of all remaining
  
  if(length(randomised) <= (n_funded - n_funded_outright)) {			# slight correction, to extend the randomisation at random
    
    randomised <- rep("randomised", times = ((n_funded - n_funded_outright) + max(rpois(1, 1), 1))) # hardcode the (small) Poisson value here 
    
  }
  
  # create a vector that will become the column indicating the decision process, of the right length
  outright_randomised <- c(funded_outright, randomised)
  if(length(outright_randomised) < nrow(meeting_proposals)) {	# pad with rejected_outright outcomes
    
    rejected_outright <- rep("rejected_outright", times = nrow(meeting_proposals) - length(outright_randomised))
    process <- c(outright_randomised, rejected_outright)
    
  } else {
    
    process <- c(outright_randomised)		# this describes meetings where no proposals were rejected outright
    
  }
  
  process <- process[1:nrow(meeting_proposals)]		# to be sure that the right number of rows is created
  meeting_proposals$process <- process
  meeting_proposals$process[is.na(meeting_proposals$process)] <- "rejected_outright"
  
  meeting_proposals$outcome <- c(rep("successful", times = n_funded), rep("rejected", times = nrow(meeting_proposals) - n_funded)) # add in an outcome
  meeting_proposals$outcome[meeting_proposals$process == "randomised"] <-
    sample(meeting_proposals$outcome[meeting_proposals$process == "randomised"], replace = FALSE) # randomise just the randomised outcomes
  
  meeting_proposals <- meeting_proposals[order(-meeting_proposals$fundability), ]	# reorder again, this time by true fundability
  
  meeting_proposals$ideal_outcome <- c(		# record the outcome that would have happened with perfect decision making based on fundability
    
    rep("successful", times = length(meeting_proposals$outcome[meeting_proposals$outcome == "successful"])),
    rep("rejected", times = length(meeting_proposals$outcome[meeting_proposals$outcome != "successful"]))
  )
  
  return(meeting_proposals)
  
}


## another function to calculate numbers of interest for each meeting
## this calculates a lot of metrics, but most aren't used in the analysis


meeting_outcomes <- function(data) {
  
  
  mean_bias <- mean(data$mean_proposal_bias)
  mean_fundability <- mean(data$fundability)
  mean_fundability_of_funded <- mean(data$fundability[data$outcome == "successful"])
  perc_randomised <- length(data$process[data$process == "randomised"]) / nrow(data)
  meeting_award_rate <- length(data$outcome[data$outcome == "successful"]) / length(data$outcome)
  
  TP <- length(data$outcome[data$outcome == "successful" & data$ideal_outcome == "successful"])
  FP <- length(data$outcome[data$outcome == "successful" & data$ideal_outcome == "rejected"])
  TN <- length(data$outcome[data$outcome == "rejected" & data$ideal_outcome == "rejected"])
  FN <- length(data$outcome[data$outcome == "rejected" & data$ideal_outcome == "successful"])
  
  TPR <- TP / (TP + FP)				# true positive rate, AKA recall or senitivity
  TNR <- TN / (TN + FN)				# true negative rate, AKA specificity
  FPR <- FP / (FP + TN)				# false positive rate, AKA type 1 error rate
  accuracy <- (TP + TN) / (TP + TN + FP + FN)	# accuracy
  balanced_accuracy <- (TPR + TNR)/2		# balanced accuracy
  F1 <- (2*TP) / (2*TP + FP + FN)			# F1 score
  precision <- TP/ (TP + FP)			# precision, AKA positive predictive value
  
  n_funded <- length(data$outcome[data$outcome == "successful"])
  
  if(n_funded == nrow(data)) {
    
    result <- NULL			# in case this happens - 'result' is the final output
    
  } else {
    
    result <- data.frame(
      
      n_proposals = nrow(data),
      mean_bias = 1 - mean_bias, 		# note the switch around so that (logically) a higher value indicates more bias
      mean_fundability = mean_fundability,
      mean_fundability_of_funded = mean_fundability_of_funded,
      perc_randomised = perc_randomised,
      meeting_award_rate = meeting_award_rate,
      true_positives = TP,
      true_negatives = TN,
      false_positives = FP,
      false_negatives = FN,
      TPR = TPR,
      TNR = TNR,
      FPR = FPR,
      accuracy = accuracy,
      balanced_accuracy = balanced_accuracy,
      F1 = F1,
      precision = precision
      
    )
  }
  
  return(result)
  
}



## Using those two functions, generate a large number of meetings with proposals sampled from the set created earlier.
## Store the meetings and their summaries.
## It's handy to be able to look at the meetings, for reassurance that they look like real meetings - but they aren't used in the analysis


meetings <- vector(mode = "list", length = meeting_count)
meeting_summaries <- vector(mode = "list", length = meeting_count)


for(i in 1:meeting_count) {
  
  if(i %% 100 == 0) message(paste0("Generating meeting ", i, " of ", meeting_count))
  this_meeting <- create_meeting(30, 6, 14, 0.5)			# NB this is where the parameters are built in to the simulation
  meetings[[i]] <- this_meeting					# only run if you want to see the meetings directly
  meeting_summaries[[i]] <- meeting_outcomes(this_meeting)
  
}


meeting_summaries <- ldply(meeting_summaries, .id = NULL)


## That's the end of the data generation for this type of simulation, the rest is analysis
## how does mean fundability of funded proposals vary with F1?


fundability_vs_f1 <- ggplot(meeting_summaries[sample(1:nrow(meeting_summaries), nrow(meeting_summaries/100)), ], aes(x = F1, y = mean_fundability_of_funded)) +
  geom_point(alpha = 0.05, colour = "#2D2E52") +
  geom_smooth(
    method = "lm",
    color = "#FF9D1B",
    show.legend = FALSE) +
  theme_minimal() +
  scale_y_continuous(limits = c(0.4, 0.9)) +
  labs(	
    x = "\nF1",
    y = "Mean fundability of funded proposals\n"
  ) +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(colour = "grey44", size = 14),
    axis.text = element_text(colour = "grey66", size = 10),
    strip.text = element_text(size = 12, colour = "white", face = "bold"),
    plot.background = element_rect(fill = "#F3F2F1", colour = NA),
    strip.background = element_rect(fill = "black", colour = "black"),
    legend.text = element_text(colour = "grey44"),
    legend.title = element_text(colour = "grey44", vjust = 0.5)
  )

fundability_vs_f1

ggsave("11_fundability_of_funded_vs_F1.pdf", width = plot_width, height = plot_height)
ggsave("11_fundability_of_funded_vs_F1.svg", width = plot_width, height = plot_height)
ggsave("11_fundability_of_funded_vs_F1.jpg", width = plot_width, height = plot_height)	# other file types are very large


## lm of this


fundability_vs_f1_model <- lm(data = meeting_summaries, mean_fundability_of_funded ~ F1)
summary(fundability_vs_f1_model)
fundability_range <- predict(fundability_vs_f1_model, newdata = data.frame(F1 = c(0, 1)))
fundability_decline <- fundability_range[2] - fundability_range[1]


## standard deviation of the mean fundability of funded proposals
## this will be used to frame the decline in mean fundability as F1 declines.


mfof_sd <- sd(meeting_summaries$mean_fundability_of_funded)
mfof_mean <- mean(meeting_summaries$mean_fundability_of_funded)
mfof_ratio <- fundability_decline / mfof_sd
mfof_ratio


## typically this will be about 1.5 sd's
## to visualise data and how it varies with the level of randomisation and biase, we want to create a grid covering the data in desired increments
## the place in the sapce created that is occupied by each meeting is added to 'meeting_summaries'


bias_increment <- 0.004
rand_increment <- 0.008

bias_range <- seq(from = floor(min(meeting_summaries$mean_bias) * 100)/100, to = ceiling(max(meeting_summaries$mean_bias) * 100)/100, by = bias_increment)
rand_range <- seq(from = floor(min(meeting_summaries$perc_randomised) * 100)/100, to = ceiling(max(meeting_summaries$perc_randomised) * 100)/100,
                  by = rand_increment)

meeting_summaries$bias_cut <- cut(meeting_summaries$mean_bias, breaks = c(-Inf, bias_range, Inf))
meeting_summaries$randomisation_cut <- cut(meeting_summaries$perc_randomised, breaks = c(-Inf, rand_range, Inf))


## create chart data


chart_data <- meeting_summaries[, c("bias_cut", "randomisation_cut", "TPR", "accuracy", "F1")]
chart_data$bias_cut <- as.character(chart_data$bias_cut)
chart_data$randomisation_cut <- as.character(chart_data$randomisation_cut)
chart_data <- ddply(chart_data, c("bias_cut", "randomisation_cut"), summarise,
                    mean_TPR = mean(TPR), mean_accuracy = mean(accuracy), mean_F1 = mean(F1), count = length(TPR))
chart_data$bias_centre <- gsub(",.*$", "", chart_data$bias_cut)
chart_data$bias_centre <- as.numeric(gsub("\\(", "", chart_data$bias_centre)) + bias_increment/2
chart_data$randomisation_centre <- gsub(",.*$", "", chart_data$randomisation_cut)
chart_data$randomisation_centre <- as.numeric(gsub("\\(", "", chart_data$randomisation_centre)) + rand_increment/2


## lengthen so that it can be done as a single faceted chart


chart_data_long <- chart_data[ , c("randomisation_centre", "bias_centre", "mean_F1", "mean_accuracy", "count")]
chart_data_long <- melt(chart_data_long, id.vars = c("randomisation_centre", "bias_centre", "count"))
chart_data_long$variable <- ifelse(chart_data_long$variable == "mean_F1", "F1 score", "Accuracy")


decision_reliability_plot <- ggplot(chart_data_long, aes(x = bias_centre, y = randomisation_centre, fill = value)) +
  geom_tile() +
  facet_wrap( ~ variable) +
  scale_fill_viridis(option = "magma",
                     guide = guide_colorbar(	direction = "horizontal",
                                             barheight = unit(2, units = "mm"),
                                             barwidth = unit(70, units = "mm"),
                                             draw.ulim = FALSE,
                                             draw.llim = FALSE,
                                             position = "bottom",
                                             panel.spacing = unit(2, "lines")
                     )) +
  theme_minimal() +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(	x = "\nDecision bias (average % reduction in unbiased score)",
        y = "Proposals randomised\n",
        fill = "Metric\nvalue   \n") +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(colour = "grey44", size = 14),
    axis.text = element_text(colour = "grey66", size = 10),
    strip.text = element_text(size = 12, colour = "white", face = "bold"),
    plot.background = element_rect(fill = "#F3F2F1", colour = NA),
    strip.background = element_rect(fill = "black", colour = "black"),
    legend.text = element_text(colour = "grey44"),
    legend.title = element_text(colour = "grey44", vjust = 0.5)
  )

decision_reliability_plot

ggsave("01_decision_reliability_heatmap.pdf", width = plot_width, height = plot_height)
ggsave("01_decision_reliability_heatmap.svg", width = plot_width, height = plot_height)


alt_decision_reliability_plot <- ggplot(chart_data_long, aes(x = randomisation_centre, y = value)) +
  geom_point(alpha = 0.05, colour = "#2D2E52") +
  geom_smooth(
    method = "lm",
    mapping = aes(weight = count), 
    color = "#FF9D1B",
    show.legend = FALSE) +
  facet_wrap( ~ variable) +
  theme_minimal() +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(	
    x = "\nProposals randomised",
    y = ""
  ) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(colour = "grey44", size = 14),
    axis.text = element_text(colour = "grey66", size = 10),
    strip.text = element_text(size = 12, colour = "white", face = "bold"),
    plot.background = element_rect(fill = "#F3F2F1", colour = NA),
    strip.background = element_rect(fill = "black", colour = "black"),
    legend.text = element_text(colour = "grey44"),
    legend.title = element_text(colour = "grey44", vjust = 0.5)
  )

alt_decision_reliability_plot

ggsave("02_decision_reliability_regression.pdf", width = plot_width, height = plot_height)
ggsave("02_decision_reliability_regression.svg", width = plot_width, height = plot_height)




#########################################################################
## similar concept to before, but this time looking for success rate differences across groups in simulated meetings
## the only difference is that this needs the 'applicant_type' as it does not sue a mix of reviewer types
## create a new function to do this, based on 'create_meeting'
## but could probably adapt the earlier function to indicate the use case, and so use only one function


create_biased_meeting <- function(number_of_proposals, rate_shape_1, rate_shape_2, frac_funded_outright) {
  
  meeting_size <- rpois(1, number_of_proposals)					# sample proposals
  meeting_proposals <- biased_proposals[sample(nrow(biased_proposals), meeting_size),
                                        c("mean_proposal_score", "mean_proposal_bias", "applicant_type", "fundability")]
  award_rate <- rbeta(1, rate_shape_1, rate_shape_2)				# determine meeting award rate
  n_funded <- round(award_rate * nrow(meeting_proposals), digits = 0)		# determine how many are funded overall
  n_funded_outright <- round(n_funded * frac_funded_outright, digits = 0)		# determine how many funded outright
  meeting_proposals <- meeting_proposals[order(-meeting_proposals$mean_proposal_score), ]	# order proposals by their average score
  
  # create col with decision process values
  # first, name those funded outright
  
  funded_outright <- rep("funded_outright", times = n_funded_outright)		
  
  # then those randomised
  randomised <- rep("randomised", times = runif(1) * (nrow(meeting_proposals) - n_funded_outright))	# random % of all remaining
  if(length(randomised) <= (n_funded - n_funded_outright)) {			# slight correction, to extend the randomisation at random
    
    randomised <- rep("randomised", times = ((n_funded - n_funded_outright) + max(rpois(1, 1), 1))) # hardcode the Poisson value here 
    
  }
  
  # create a vector that will become a column indicating the process, of the right length
  outright_randomised <- c(funded_outright, randomised)
  if(length(outright_randomised) < nrow(meeting_proposals)) {	# pad with rejected_outright outcomes
    
    rejected_outright <- rep("rejected_outright", times = nrow(meeting_proposals) - length(outright_randomised))
    process <- c(outright_randomised, rejected_outright)
    
  } else {
    
    process <- c(outright_randomised)
    
  }
  
  process <- process[1:nrow(meeting_proposals)]		# to be sure that the right number of rows is created
  meeting_proposals$process <- process
  meeting_proposals$process[is.na(meeting_proposals$process)] <- "rejected_outright"
  
  meeting_proposals$outcome <- c(rep("successful", times = n_funded), rep("rejected", times = nrow(meeting_proposals) - n_funded)) # add in an outcome
  meeting_proposals$outcome[meeting_proposals$process == "randomised"] <-
    sample(meeting_proposals$outcome[meeting_proposals$process == "randomised"], replace = FALSE) # randomise just the randomised outcomes
  
  meeting_proposals <- meeting_proposals[order(-meeting_proposals$fundability), ]	# reorder again, this time by true fundability
  
  meeting_proposals$ideal_outcome <- c(		# record the outcome that would have happened with perfect decision making based on fundability
    
    rep("successful", times = length(meeting_proposals$outcome[meeting_proposals$outcome == "successful"])),
    rep("rejected", times = length(meeting_proposals$outcome[meeting_proposals$outcome != "successful"]))
  )
  
  return(meeting_proposals)
  
}


biased_meeting_outcomes <- function(data) {
  
  
  ## measures of interest are the mean bias of the proposals at the meeting, the difference in success rates, the % randomised
  ## and the F1 measure/accuracy
  
  perc_randomised <- length(data$process[data$process == "randomised"]) / nrow(data)
  meeting_award_rate <- length(data$outcome[data$outcome == "successful"]) / length(data$outcome)
  mean_proposal_bias <- mean(data$mean_proposal_bias)
  
  award_rate_difference <-		# rate of applicants not subject to bias - rate of applicants subject to bias
    
    (length(data$applicant_type[data$outcome == "successful" & data$applicant_type == "not_subject_to_bias"]) / 
       length(data$applicant_type[data$applicant_type == "not_subject_to_bias"]) ) -
    (length(data$applicant_type[data$outcome == "successful" & data$applicant_type == "subject_to_bias"]) / 
       length(data$applicant_type[data$applicant_type == "subject_to_bias"]) )
  
  TP <- length(data$outcome[data$outcome == "successful" & data$ideal_outcome == "successful"])
  FP <- length(data$outcome[data$outcome == "successful" & data$ideal_outcome == "rejected"])
  TN <- length(data$outcome[data$outcome == "rejected" & data$ideal_outcome == "rejected"])
  FN <- length(data$outcome[data$outcome == "rejected" & data$ideal_outcome == "successful"])
  
  accuracy <- (TP + TN) / (TP + TN + FP + FN)	# accuracy
  F1 <- (2*TP) / (2*TP + FP + FN)			# F1 score
  
  
  n_funded <- length(data$outcome[data$outcome == "successful"])
  
  if(n_funded == nrow(data)) {
    
    result <- NULL			# in case this happens - 'result' is the final output
    
  } else {
    
    result <- data.frame(
      
      n_proposals = nrow(data),
      perc_randomised = perc_randomised,
      meeting_award_rate = meeting_award_rate,
      award_rate_difference = award_rate_difference,
      mean_proposal_bias = mean_proposal_bias,
      F1 = F1,
      accuracy = accuracy
      
    )
  }
  
  return(result)
  
}


## as before, generate list of meetings nd their summaries and turn into df


biased_meetings <- vector(mode = "list", length = meeting_count)
biased_meeting_summaries <- vector(mode = "list", length = meeting_count)


for(i in 1:meeting_count) {
  
  if(i %% 100 == 0) message(paste0("Generating biased meeting ", i, " of ", meeting_count))
  this_meeting <- create_biased_meeting(30, 6, 14, 0.5)	# again, the relevant meeting values are added here
  biased_meetings[[i]] <- this_meeting		# only run if you want to see the meetings directly
  biased_meeting_summaries[[i]] <- biased_meeting_outcomes(this_meeting)
  
}


biased_meeting_summaries <- ldply(biased_meeting_summaries, .id = NULL)
av_award_rate_diff <- mean(biased_meeting_summaries$award_rate_difference)


rate_diffs_by_randomisation <- ggplot(biased_meeting_summaries, aes(x = award_rate_difference*100, y = perc_randomised)) +
  geom_density_2d_filled(bins = 90, show.legend = FALSE) +
  scale_fill_viridis(discrete = TRUE, option = "magma") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red", linewidth = 0.6) +
  #geom_vline(xintercept = -0.25, linetype = "dotted", colour = "red") +
  #geom_vline(xintercept = 0.25, linetype = "dotted", colour = "red") +
  theme_minimal() +
  scale_x_continuous() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(	x = "\nAward rate difference (percentage points)",
        y = "Meeting proposals subject to randomisation\n") +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(colour = "grey44", size = 14),
    axis.text = element_text(colour = "grey66", size = 10),
    panel.background = element_rect(fill = "#F3F2F1", colour = NA),
    plot.background = element_rect(fill = "#F3F2F1", colour = NA)
  )

rate_diffs_by_randomisation

ggsave("03_rate_diffs_by_randomisation.pdf", width = plot_width, height = plot_height)
ggsave("03_rate_diffs_by_randomisation.svg", width = plot_width, height = plot_height)


bias_randomisation_reliability_plot <- ggplot(biased_meeting_summaries , aes(x = F1, y = perc_randomised, colour = award_rate_difference)) +
  geom_point(alpha = 0.1) +
  scale_colour_gradient2(	low = "#2E2D52", high = "#D77900",
                          name = "Award rate difference",
                          guide = guide_colorbar(	direction = "horizontal",
                                                  barheight = unit(2, units = "mm"),
                                                  barwidth = unit(70, units = "mm"),
                                                  draw.ulim = FALSE,
                                                  draw.llim = FALSE,
                                                  position = "bottom",
                                                  panel.spacing = unit(2, "lines")
                          )) +
  theme_minimal() +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(	x = "\nF1",
        y = "% randomised\n",
        fill = "Metric\nvalue   \n") +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(colour = "grey44", size = 14),
    axis.text = element_text(colour = "grey66", size = 10),
    strip.text = element_text(size = 12, colour = "white", face = "bold"),
    plot.background = element_rect(fill = "#F3F2F1", colour = NA),
    panel.background = element_rect(fill = "#F3F2F1", colour = NA),
    strip.background = element_rect(fill = "black", colour = "black"),
    legend.text = element_text(colour = "grey44"),
    legend.title = element_text(colour = "grey44", vjust = 0.5)
  )

bias_randomisation_reliability_plot


ggsave("04_award_rate_difference_by_perc_randomisation_and_F1.pdf", width = plot_width, height = plot_height)
ggsave("04_award_rate_difference_by_perc_randomisation_and_F1.svg", width = plot_width, height = plot_height)


########################################################################################################################################################
## the final simulation: how does the decision process reliability (F1) vary as the % randomised and the bias vary?
## to do this we need to go right back to the start and systematically vary both % randomsiation and bias in proposal reviews
## yes, this is going to need nested for loops


randomisation_percents <- seq(from = 0, to = 1, by = 0.05)	# for testing, make these increments larger
bias_percents <- seq(from = 0, to = 1, by = 0.05)

## for each combination of randomisation and bias percents we need to generate proposals with that bias,
## assemble them into meetings with that level of randomisation
## calculate the F1 for each meeting
## store the relevant bias, randomisation level and F1 for each meeting

proposal_sample_length <- 1e5		# the number of proposals to create for each level of bias and randomisation
meeting_sample_length <- 1e4		# the number of meetings, sampling from the proposals, to create for each level of bias and randomisation
number_of_proposals_in_meeting <- 30	# average number of proposal in a meeting
frac_funded_outright <- 0.5		# percent of funded proposals that is funded outright


## store results in a matrix, one each for F1 and accuracy
## columns are bias percents, rows are randomisation percents

F1_results_matrix <- matrix(nrow = length(randomisation_percents), ncol = length(bias_percents))
accuracy_results_matrix <- matrix(nrow = length(randomisation_percents), ncol = length(bias_percents))

for(i in 1:length(randomisation_percents)) {
  
  for (j in 1:length(bias_percents)) {
    
    proposal_sample <- vector(mode = "list", length = proposal_sample_length)	# empty list for results of proposal generation
    
    message("Generating sample of ", proposal_sample_length, " proposals, for meetings with ",
            bias_percents[j], " bias and ", round(100*randomisation_percents[i], digits = 0), "% randomisation")
    
    for(k in 1:proposal_sample_length) {
      
      review_count <- rpois(1, 3)			# determine how many reviews to generate
      if(review_count < 3) review_count <- 3		# have at least 3 though
      
      result <- score_generator(min_shape1, max_shape1, min_shape2, max_shape2, review_count, bias_percents[j])
      result$scores[is.na(result$scores)] <- 1		# some values of i,j produce NAs, but the minimum score is 1
      
      # decide whether to take a proposal that is subject to bias or one that is not subject to bias
      # NB could do it with a mix within the same proposal instead, but would need to extract different data (a 'mix')
      
      determine_group <- runif(1)
      if(determine_group <= 0.5) {				
        result <- result[which(result$type == "biased"),]
        applicant_type <- "subject_to_bias"
      } else {
        result <- result[which(result$type == "unbiased"),]
        applicant_type <- "not_subject_to_bias"
      }
      
      proposal_sample[[k]] <- data.frame(	n_reviews = nrow(result),
                                          mean_score = mean(result$scores),
                                          applicant_type = applicant_type,
                                          underlying_bias = bias_percents[j],
                                          actual_fundability = unique(result$actual_fundability)
      )
      
    }
    
    proposal_sample <- ldply(proposal_sample)	# the pool of proposals from which we can sample for this iteration
    
    meeting_sample <- vector(mode = "list", length = meeting_sample_length)		# empty vector for meetings
    
    for(q in 1:length(meeting_sample)) {		# can't use create meeting here unfortunately
      
      # first, set up the main elements of the meeting
      
      meeting_size <- rpois(1, number_of_proposals_in_meeting)				
      meeting_proposals <- proposal_sample[sample(nrow(proposal_sample), meeting_size), ]
      award_rate <- rbeta(1, 6, 14)			# NB Manual addition of success rate beta parameters!		
      n_funded <- round(award_rate * nrow(meeting_proposals), digits = 0)		
      frac_randomised <- randomisation_percents[i]
      n_randomised <- round(frac_randomised * nrow(meeting_proposals), digits = 0)
      if(n_randomised == 1) n_randomised <- 2					# can't randomise without at least two options
      n_not_randomised <- nrow(meeting_proposals) - n_randomised			# comprises those funded outright and those rejected outright
      n_rejected_outright <- n_not_randomised - n_funded
      if(n_rejected_outright < 0) n_rejected_outright <- 0
      
      meeting_proposals <- meeting_proposals[order(-meeting_proposals$mean_score), ]
      
      # the challenge here is to work out a way to determine how many randomised proposals to fund
      # there are all sorts of subtleties that need to be dealt with individually, so this is very manual and could be done differently
      # first, the case when all proposals are randomised
      
      if(n_randomised == nrow(meeting_proposals)) {
        
        meeting_proposals$process <- "randomised"
        meeting_proposals <- meeting_proposals[sample(1:nrow(meeting_proposals)), ]
        meeting_proposals$outcome <- c(rep("successful", times = n_funded), rep("rejected", times = nrow(meeting_proposals) - n_funded))
        meeting_proposals <- meeting_proposals[order(-meeting_proposals$actual_fundability), ]
        meeting_proposals$ideal_outcome <- c(rep("successful", times = n_funded), rep("rejected", times = nrow(meeting_proposals) - n_funded))
        
      } else if(n_randomised == 0) {
        
        meeting_proposals$process <- "fully_ranked"
        meeting_proposals$outcome <- c(rep("successful", times = n_funded), rep("rejected", times = nrow(meeting_proposals) - n_funded))
        meeting_proposals <- meeting_proposals[order(-meeting_proposals$actual_fundability), ]
        meeting_proposals$ideal_outcome <- c(rep("successful", times = n_funded), rep("rejected", times = nrow(meeting_proposals) - n_funded))
        
      } else if(n_funded == 1) {
        
        meeting_proposals$process <- c("funded_outright", rep("rejected_outright", times = nrow(meeting_proposals) - n_funded))
        meeting_proposals$outcome <- c("successful", rep("rejected", times = nrow(meeting_proposals) - n_funded))
        meeting_proposals <- meeting_proposals[order(-meeting_proposals$actual_fundability), ]
        meeting_proposals$ideal_outcome <- c("successful", rep("rejected", times = nrow(meeting_proposals) -n_funded))
        
      } else if(n_funded %in% 2:3) {	
        
        if(n_rejected_outright == 0) {
          
          process <- c(	rep("funded_outright", times = 1),
                        rep("randomised", times = nrow(meeting_proposals) - 1)
          )
          
        } else {
          
          process <- c(	rep("funded_outright", times = 1),
                        rep("randomised", times = nrow(meeting_proposals) - 1 - n_rejected_outright),
                        rep("rejected_outright", times = n_rejected_outright)
          )
        }
        
        if(length(process) < nrow(meeting_proposals)) process <-
            c(process, rep("rejected_outright", times = nrow(meeting_proposals) - length(process)) )
        if(length(process) > nrow(meeting_proposals)) process <- process[1:nrow(meeting_proposals)]
        
        meeting_proposals$process <- process
        meeting_proposals$outcome <- c(rep("successful", times = n_funded), rep("rejected", times = nrow(meeting_proposals) - n_funded))
        meeting_proposals$outcome[meeting_proposals$process =="randomised"] <- sample(meeting_proposals$outcome[meeting_proposals$process == "randomised"])
        meeting_proposals <- meeting_proposals[order(-meeting_proposals$actual_fundability), ]
        meeting_proposals$ideal_outcome <- c(rep("successful", times = n_funded), rep("rejected", times = nrow(meeting_proposals) - n_funded))
        
      } else if(n_funded >= 4 & n_funded %% 2 == 0) {		# at least 4 funded, and an even number
        
        if(n_rejected_outright == 0) {
          
          process <- c(	rep("funded_outright", times = n_funded/2),
                        rep("randomised", times = n_randomised)
          )
          
        } else {
          
          process <- c(	rep("funded_outright", times = n_funded/2),
                        rep("randomised", times = n_randomised),
                        rep("rejected_outright", times = n_rejected_outright)
          )
          
        }
        
        if(length(process) < nrow(meeting_proposals)) process <-
            c(process, rep("rejected_outright", times = nrow(meeting_proposals) - length(process)) )
        if(length(process) > nrow(meeting_proposals)) process <- process[1:nrow(meeting_proposals)]
        
        meeting_proposals$process <- process
        meeting_proposals$outcome <- c(rep("successful", times = n_funded), rep("rejected", times = nrow(meeting_proposals) - n_funded))
        meeting_proposals$outcome[meeting_proposals$process =="randomised"] <- sample(meeting_proposals$outcome[meeting_proposals$process == "randomised"])
        meeting_proposals <- meeting_proposals[order(-meeting_proposals$actual_fundability), ]
        meeting_proposals$ideal_outcome <- c(rep("successful", times = n_funded), rep("rejected", times = nrow(meeting_proposals) - n_funded))
        
        
      } else if(n_funded >= 4 & n_funded %% 2 != 0) {		# at least 4 funded, and an odd number
        
        process <- c(	rep("funded_outright", times = round(n_funded/2)),	# note R uses the banker's round, so this is unbiased
                      rep("randomised", times = n_randomised),
                      rep("rejected_outright", times = n_rejected_outright)
        )
        
        if(length(process) < nrow(meeting_proposals)) process <-
            c(process, rep("rejected_outright", times = nrow(meeting_proposals) - length(process)) )
        
        if(length(process) > nrow(meeting_proposals)) process <- process[1:nrow(meeting_proposals)]
        
        
        meeting_proposals$process <- process
        meeting_proposals$outcome <- c(rep("successful", times = n_funded), rep("rejected", times = nrow(meeting_proposals) - n_funded))
        meeting_proposals$outcome[meeting_proposals$process =="randomised"] <- sample(meeting_proposals$outcome[meeting_proposals$process == "randomised"])
        meeting_proposals <- meeting_proposals[order(-meeting_proposals$actual_fundability), ]
        meeting_proposals$ideal_outcome <- c(rep("successful", times = n_funded), rep("rejected", times = nrow(meeting_proposals) - n_funded))
        
      } else {
        
        meeting_proposals <- NULL
        
      }
      
      
      
      meeting_sample[[q]] <- meeting_proposals
      
      if(q %% 1000 == 0) message("Generating meeting ", q, " with ",
                                 bias_percents[j], " bias and ", round(100*randomisation_percents[i], digits = 0), "% randomisation")
      
    }
    
    
    meeting_sample <- lapply(meeting_sample, function(x) {
      
      TP <- length(x$outcome[x$outcome == "successful" & x$ideal_outcome == "successful"])
      FP <- length(x$outcome[x$outcome == "successful" & x$ideal_outcome == "rejected"])
      TN <- length(x$outcome[x$outcome == "rejected" & x$ideal_outcome == "rejected"])
      FN <- length(x$outcome[x$outcome == "rejected" & x$ideal_outcome == "successful"])
      
      
      accuracy <- (TP + TN) / (TP + TN + FP + FN)	# accuracy
      F1 <- (2*TP) / (2*TP + FP + FN)			# F1 score
      
      bias <- mean(x$underlying_bias)
      perc_randomised <- length(x$process[x$process == "randomised"]) / nrow(x)
      F1 <- F1
      accuracy <- accuracy
      
      if(length(bias) != 1) bias <- NA
      if(length(perc_randomised) != 1) perc_randomised <- NA
      if(length(F1) != 1) F1 <- NA
      if(length(accuracy) != 1) accuracy <- NA
      
      return(data.frame(bias = bias, perc_randomised = perc_randomised, F1 = F1, accuracy = accuracy))
      
      
    })
    
    meeting_sample <- ldply(meeting_sample)
    
    ## finally, place the resutls in the relevant place in the matrix created right at the start
    
    F1_results_matrix[i,j] <- mean(meeting_sample$F1, na.rm = TRUE)
    accuracy_results_matrix[i,j] <- mean(meeting_sample$accuracy, na.rm = TRUE)
    
    
  }	# bias loop (j) closed
}	# randomisation percents loop (i) closed, and process ends


## reshape data into a plottable form


F1_results_df <- as.data.frame.matrix(F1_results_matrix)
names(F1_results_df) <- as.numeric(unlist(bias_percents))
F1_results_df$randomisation <- as.numeric(unlist(randomisation_percents))
F1_results_df <- melt(F1_results_df, id.vars = c("randomisation"))
F1_results_df$randomisation <- as.numeric(F1_results_df$randomisation)
F1_results_df$variable <- as.numeric(as.character(F1_results_df$variable))

F1_vs_bias_vs_randomisation_plot <- ggplot(F1_results_df, aes(x = randomisation, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_viridis(option = "magma",
                     guide = guide_colorbar(	direction = "horizontal",
                                             barheight = unit(2, units = "mm"),
                                             barwidth = unit(70, units = "mm"),
                                             draw.ulim = FALSE,
                                             draw.llim = FALSE,
                                             position = "bottom",
                                             panel.spacing = unit(2, "lines")
                     )) +
  theme_minimal() +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(	x = "\nRandomisation applied",
        y = "Biased scores as % of unbiased scores\n",
        fill = "F1  \n") +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(colour = "grey44", size = 14),
    axis.text = element_text(colour = "grey66", size = 10),
    strip.text = element_text(size = 12, colour = "white", face = "bold"),
    plot.background = element_rect(fill = "#F3F2F1", colour = NA),
    strip.background = element_rect(fill = "black", colour = "black"),
    legend.text = element_text(colour = "grey44"),
    legend.title = element_text(colour = "grey44", vjust = 0.5)
  )

F1_vs_bias_vs_randomisation_plot

ggsave("05_variation_of_F1_with_bias_and_randomisation.pdf", width = plot_width, height = plot_height)
ggsave("05_variation_of_F1_with_bias_and_randomisation.svg", width = plot_width, height = plot_height)


accuracy_results_df <- as.data.frame.matrix(accuracy_results_matrix)
names(accuracy_results_df) <- as.numeric(unlist(bias_percents))
accuracy_results_df$randomisation <- as.numeric(unlist(randomisation_percents))
accuracy_results_df <- melt(accuracy_results_df, id.vars = c("randomisation"))
accuracy_results_df$randomisation <- as.numeric(accuracy_results_df$randomisation)
accuracy_results_df$variable <- as.numeric(as.character(accuracy_results_df$variable))


accuracy_vs_bias_vs_randomisation_plot <- ggplot(accuracy_results_df, aes(x = randomisation, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_viridis(option = "magma",
                     guide = guide_colorbar(	direction = "horizontal",
                                             barheight = unit(2, units = "mm"),
                                             barwidth = unit(70, units = "mm"),
                                             draw.ulim = FALSE,
                                             draw.llim = FALSE,
                                             position = "bottom",
                                             panel.spacing = unit(2, "lines")
                     )) +
  theme_minimal() +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(	x = "\nRandomisation applied",
        y = "Biased scores as % of unbiased scores\n",
        fill = "Accuracy   \n") +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(colour = "grey44", size = 14),
    axis.text = element_text(colour = "grey66", size = 10),
    strip.text = element_text(size = 12, colour = "white", face = "bold"),
    plot.background = element_rect(fill = "#F3F2F1", colour = NA),
    strip.background = element_rect(fill = "black", colour = "black"),
    legend.text = element_text(colour = "grey44"),
    legend.title = element_text(colour = "grey44", vjust = 0.5)
  )

accuracy_vs_bias_vs_randomisation_plot

ggsave("06_variation_of_accuracy_with_bias_and_randomisation.pdf", width = plot_width, height = plot_height)
ggsave("06_variation_of_accuracy_with_bias_and_randomisation.svg", width = plot_width, height = plot_height)


###############################################################################################
## plot showing typical reviewer scores 


example_proposals <- data.frame( 	shape1_val = sample(2:20, 16, replace = TRUE),
                                  shape2_val = sample(4:7, 16, replace = TRUE)
)
example_proposals_list <- vector(mode = "list", length = nrow(example_proposals))

for(i in 1:nrow(example_proposals)) {
  
  example_proposals_list[[i]] <- data.frame(	sample = i,
                                             x = seq(from = 0.01, to = 0.99, by = 0.01),
                                             vals = 	dbeta(seq(from = 0.01, to = 0.99, by = 0.01),
                                                           shape1 = example_proposals$shape1_val[i],
                                                           shape2 = example_proposals$shape2_val[i]),
                                             alpha = example_proposals$shape1_val[i],
                                             beta = example_proposals$shape2_val[i]
  )
  
}

example_proposals_list <- ldply(example_proposals_list, .id = NULL)
example_proposals_list$ind <- paste0("Shape 1 = ", example_proposals_list$alpha, "; Shape 2 = ", example_proposals_list$beta)

proposals_caption <- paste0("\nShape 1 parameter allowed to vary between ", min_shape1, " and ", max_shape1,
                            "; Shape 2 parameter allowed to vary between ",  min_shape2, " and ", max_shape2)

example_proposals_plot <- ggplot(example_proposals_list, aes(x = x, y = vals, group = 1)) +
  geom_line(colour = "#FF5A5A") +
  facet_wrap( ~ ind) +
  theme_minimal() +
  scale_x_continuous(breaks = c(0, 1/6, 1/3, 0.5, 2/3, 5/6, 1), labels = (c("", 1:6))) +
  labs(	x = "",
        y = "",
        caption = proposals_caption
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.1, colour = "grey77"),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(colour = "grey44", size = 14),
    axis.text.y = element_blank(),
    axis.text.x = element_text(hjust = 2.5),
    strip.text = element_text(size = 8, colour = "white", face = "bold"),
    plot.background = element_rect(fill = "#F3F2F1", colour = NA),
    strip.background = element_rect(fill = "black", colour = "black"),
    legend.text = element_text(colour = "grey44"),
    legend.title = element_text(colour = "grey44", vjust = 0.5),
    plot.caption = element_text(size = 6, colour = "grey44")
  )

example_proposals_plot


ggsave("09_sample_proposals.pdf", width = plot_width, height = plot_height)
ggsave("09_sample_proposals.svg", width = plot_width, height = plot_height)


## plot shoiwng meeting success rate distribution
## NB this sues the values hardwired into the functions above, so if they change this needs to change


rate_def <- data.frame(		x = seq(from = 0.01, to = 0.99, by = 0.01),
                         vals = 	dbeta(seq(from = 0.01, to = 0.99, by = 0.01),
                                       shape1 = 6,
                                       shape2 = 14)
)


success_rates_plot <- ggplot(rate_def, aes(x = x, y = vals, group = 1)) +
  geom_line() +
  theme_minimal() +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(	x = "",
        y = ""
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.2, colour = "grey77"),
    panel.grid.minor.x = element_line(linewidth = 0.1, colour = "grey88"),
    axis.title = element_text(colour = "grey44", size = 14),
    axis.text.y = element_blank(),
    axis.text.x = element_text(),
    strip.text = element_text(size = 8, colour = "white", face = "bold"),
    plot.background = element_rect(fill = "#F3F2F1", colour = NA),
    strip.background = element_rect(fill = "black", colour = "black"),
    legend.text = element_text(colour = "grey44"),
    legend.title = element_text(colour = "grey44", vjust = 0.5),
    plot.caption = element_text(size = 6, colour = "grey44")
  )

success_rates_plot


ggsave("10_meeting_success_rates.pdf", width = plot_width, height = plot_height)
ggsave("10_meeting_success_rates.svg", width = plot_width, height = plot_height)