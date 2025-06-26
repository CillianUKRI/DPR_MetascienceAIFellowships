#################################################################################################
# Author: Cillian Brophy
# Date last modified: 28/04/25
# Script purpose: This script loads in the csv file which contains the input data for the DPR allocation program (main.R).
#                 In doing this, it extracts the Co-Is that also appear as applicants (not needed for fellowships).
#                 Also, the applicant pool is split into two groups randomly, and the count vectors for each group are initiated.
# Workflow and file dependencies: Run script from start to finish. Additional checks are commented out at the end.
# Format of inputdf: 6 columns, 5 character and 1 numeric ("ID"). 
#                   Reference | ApplicantName | ApplicantOrganisation | ApplicantEmail | ID | Conflicts
#           e.g.    APP123456   Joe Bloggs       University of Exeter   joebloggs@uoe...  1   None
#################################################################################################

## Load in data
## Format it correctly
## Split into two random groups

library(stringr) ## for str_trim()

# Function to extract names from the Co-I character column
extract_names <- function(data) {
  split_data <- strsplit(data, ";")[[1]]
  names <- sapply(split_data, function(x) sub(",.*", "", x))
  paste(names, collapse = ", ")
}  

inputdf <- read_csv("inputdata/ApplicationsFinal_280425.csv") |>
  rename("Title" = `Application reference`) |>
  select("Title", "Applicant Name", "Applicant Organisation", "Applicant Email") |>
  mutate(ID = seq_along(Title)) |>
  rename(ApplicantName = "Applicant Name") |>
  rename(ApplicantOrganisation = "Applicant Organisation") |>
  rename(ApplicantEmail = "Applicant Email") |>
  rename(Reference = "Title") |>
  mutate(across(everything(), ~ str_trim(., side="left"))) |> #remove all leading white space
  mutate(Conflicts = "None") |>
  mutate(ID = as.numeric(ID))

##### This section removes any Co-Is that are unique, i.e. only present for one application
##### Not needed for fellowships
# This is to 
# Split the names into individual rows
# df_split <- data.frame(
#   ID = rep(rawcsv$ID, sapply(strsplit(rawcsv$`Co-Is`, ", "), length)),
#   name = unlist(strsplit(rawcsv$`Co-Is`, ", ")),
#   stringsAsFactors = FALSE
# ) |>
#   mutate(name = trimws(name)) # This creates a long column of all names found in the Co-I field
# 
# df_filtered <- df_split[df_split$name %in% rawcsv$ApplicantName, ] # this removes any Co-I name that's not also an applicant (for matching reviewers)
# 
# # Combine the filtered names back into a columnar format
# df_filtered_combined <- aggregate(name ~ ID, data = df_filtered, paste, collapse = ", ") # Combining back to one row per applicant again. All Co-I's found in one row.
# 
# inputdf <- left_join(rawcsv, df_filtered_combined) |>
#   select(-`Co-Is`) |>
#   rename(`Co-Is` = "name") |>
#   mutate(Conflicts = "None")

# Randomly split pool into two groups for DPR allocation, to ensure no reciprocal reviewing
inputdfrandomsample <- sample(inputdf$ID, floor(nrow(inputdf)/2), replace=FALSE)
inputdfroundone <- inputdf$ID[inputdf$ID %in% inputdfrandomsample]
inputdfroundtwo <- inputdf$ID[!(inputdf$ID %in% inputdfrandomsample)]

# Create tables used for "translating" between indices
inputdfroundtwo_assignments_countdf <- data.frame(count = rep(0, length(inputdfroundtwo)),
                                                  groupindex = inputdfroundtwo,
                                                  rownumber = seq(1, length(inputdfroundtwo))) # Track how many times each person in Group 2 is assigned

inputdfroundone_assignments_countdf <- data.frame(count = rep(0, length(inputdfroundone)),
                                                  groupindex = inputdfroundone,
                                                  rownumber = seq(1, length(inputdfroundone))) # Track how many times each person in Group 1 is assigned



# Create count vectors
inputdfroundtwo_assignments_count <- rep(0, length(inputdfroundtwo)) # Track how many times each person in Group 2 is assigned
inputdfroundone_assignments_count <- rep(0, length(inputdfroundone)) # Track how many times each person in Group 1 is assigned

inputdf <- inputdf |>
  mutate(Group = case_when(ID %in% inputdfroundone_assignments_countdf$groupindex ~ "Group 1",
                           ID %in% inputdfroundtwo_assignments_countdf$groupindex ~ "Group 2",
                           TRUE ~ "ERROR")) |>
  relocate(Group, .after = everything())
