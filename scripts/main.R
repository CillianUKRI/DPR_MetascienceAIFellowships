#################################################################################################
# Author: Cillian Brophy
# Date last modified: 14/04/25
# Script purpose: This script and its dependencies are used to allocate applicants of the Metascience AI early 
#                 fellowship call for distributed peer review.
# Workflow and file dependencies: The script first splits the total pool of applicants randomly into two.
#                                 Group 1 reviews group 2 and vice versa. Never the twain shall meet...
#                                 "Constraint lists" are generated, which are the people forbidden to ever 
#                                 match with each other (conflicts or reciprocal). Then people are randomly
#                                 allocated 10 people each, while ensuring that each person gets reviewed
#                                 10 times and everybody has 10 reviews to do (or as close as possible). 
#                                 If this isn't possible in the first allocation, the assignments will be redistributed until
#                                 it is.
#################################################################################################

## IMPORTANT: THIS SCRIPT AND ALL SPECIFIED DEPENDENCY SCRIPTS WAS USED IN THE DPR ALLOCATION of 29/04/25 ## 

set.seed(122)
library(tidyverse)
library(writexl)
source("functions/load_input_data280425DPR.R")
#source("functions/make_dummy_data.R")
source("functions/DPRAllocation.R")
source("functions/constraint_list.R")
source("functions/redistribute_allocations.R")
source("functions/small_helper_functions.R")

# Generate list of forbidden people for group 1 to match with
group_one_constraint_list <- generate_constraint_list(data1 = inputdfroundone, data2 = inputdfroundtwo, mastertable = inputdf)

# Allocate group 1 10 people, with no person from group 2 being allocated more than 10 times
group1_DPR_allocation <- DPR_allocation(group_to_allocate = inputdfroundone, group_allocations_from = inputdfroundtwo,
                                        constraint_list = group_one_constraint_list,
                                        count_of_assigned = inputdfroundtwo_assignments_count, 
                                        translation_df = inputdfroundtwo_assignments_countdf)

# The matrix of allocations for group 1
group1assigned <- group1_DPR_allocation[[1]]

# The count vector used to ensure that no person from group 2 is allocated more than 10 times
inputdfroundtwo_assignments_count <- group1_DPR_allocation[[2]]
table(inputdfroundtwo_assignments_count)

# Vector of any applicant who has not been allocated 10 reviewers (likely)
# (This occurs because as people are assigned, it narrows down the available pool until by the time the
# last few applications are assigned reviewers, the available pool is so small that it's difficult to find a valid match)
if(any(is.na(group1assigned[,]))) {
  reviewers_lessthan_ten_assignees <- as.vector(which(apply(group1assigned,1,anyNA))) # Find the rows which have any NA values
  
  # If there is anyone who has not been allocated 10 reviewers, redistribute reviewers starting at applicant 1 in group 1
  # (If there is someone allocated to applicant 1 who could be allocated to the applicant missing reviewers, then swap that
  # reviewer with someone from the available pool, and so on.)
  group1assigned_redistribution <- groupallocations_redistribute(group_to_allocate = inputdfroundone, group_allocations_from = inputdfroundtwo, 
                                                                 assigned_group = group1assigned, constraint_list = group_one_constraint_list,
                                                                 count_of_assigned = inputdfroundtwo_assignments_count, 
                                                                 translation_df = inputdfroundtwo_assignments_countdf)
  
  group1assigned_redistributed <- group1assigned_redistribution[[1]]
  
  inputdfroundtwo_assignments_count <- group1assigned_redistribution[[2]]
} else {
  group1assigned_redistributed <- group1assigned # Rename this matrix so that it can be called correctly later
}

table(inputdfroundtwo_assignments_count)

# If there is more than one NA in any row (an application does not have at least 9 reviewers)
# Then take an assignee from a reviewer that does have 10, ensuring that it is a valid swap
# And if people haven't been assigned 10 reviews, swap with people who have 10 until everybody has at least 9
if(any(apply(group1assigned_redistributed, 1, function(row) sum(is.na(row)) > 1))){
  group1assigned_redistribution_subten_assignees <- redistribute_sub_ten_assignees(assigned_group_redistributed = group1assigned_redistributed, constraint_list = group_one_constraint_list) 
  group1assigned_redistributed <- group1assigned_redistribution_subten_assignees
  

} else if(any(table(inputdfroundtwo_assignments_count)<9)){
  group1assigned_redistribution_subten <- redistribute_sub_ten_assignments(assigned_group_redistributed = group1assigned_redistributed,
                                                                           constraint_list = group_one_constraint_list)
  group1assigned_redistributed <- group1assigned_redistribution_subten[[1]]
  inputdfroundtwo_assignments_count <- as.numeric(table(group1assigned_redistributed))
}

# Check that each reviewer has 9 or more to review
table(inputdfroundtwo_assignments_count)

# Group 2 allocation ------------------------------------------------------

# Generate the "list of reciprocals", which is a list of people that each person in group 2 is matched with in 
# Group 1, who should not be matched with again
list_of_reciprocals <- find_all_reciprocals_in_group(group1assigned_redistributed, inputdfroundtwo)

# Add in these reciprocals to the original "inputdf" table, in the "Conflicts" field.
for(i in names(list_of_reciprocals)){
  reciprocals_for_this_person <- list_of_reciprocals[i]
  
  names_of_reciprocals <- as.character() # Initialise empty vector
  
  for(individual_reciprocal in reciprocals_for_this_person){
    
    row_index <- which(inputdf$ID %in% individual_reciprocal)
    
    names_of_reciprocals <- paste(strsplit(names_of_reciprocals, ""), inputdf$ApplicantName[row_index], collapse=",")
  }
  row_index_conflicts <- which(inputdf$ID == as.numeric(i))
  inputdf$Conflicts[row_index_conflicts] <- trimws(names_of_reciprocals, which = "left") # remove leading white space
  
}

# Now generate the list of forbidden people that cannot match to group 2
group_two_constraint_list <- generate_constraint_list(data1 = inputdfroundtwo, data2 = inputdfroundone, mastertable = inputdf)



# Allocate people in group 2 10 people from group 1 if possible
group2_DPR_allocation <- DPR_allocation(group_to_allocate = inputdfroundtwo, group_allocations_from = inputdfroundone,
                                        constraint_list = group_two_constraint_list,
                                        count_of_assigned = inputdfroundone_assignments_count, 
                                        translation_df = inputdfroundone_assignments_countdf)

# The matrix of allocations for group 2
group2assigned <- group2_DPR_allocation[[1]]

# The count vector used to ensure that no person from group 1 is allocated more than 10 times
inputdfroundone_assignments_count <- group2_DPR_allocation[[2]]
table(inputdfroundtwo_assignments_count)
# Vector of any person who has not been allocated 10 reviewers (likely)
# (This occurs because as people are assigned, it narrows down the available pool until by the time the
# last few applications are assigned reviewers, the available pool is so small that it's difficult to find a valid match)
if(any(is.na(group2assigned[,]))) {
  reviewers_lessthan_ten_assignees <- as.vector(which(apply(group2assigned,1,anyNA))) # Find the rows which have any NA values
  
  # If there is anyone who has not been allocated 10 people, redistribute people starting at person 1 in group 2
  # (If there is someone allocated to person 1 who could be allocated to the person missing people, then swap that
  # person with someone from the available pool, and so on.)
  group2assigned_redistribution <- groupallocations_redistribute(group_to_allocate = inputdfroundtwo, group_allocations_from = inputdfroundone, 
                                                                 assigned_group = group2assigned, constraint_list = group_two_constraint_list,
                                                                 count_of_assigned = inputdfroundone_assignments_count, 
                                                                 translation_df = inputdfroundone_assignments_countdf)
  
  group2assigned_redistributed <- group2assigned_redistribution[[1]]
  inputdfroundone_assignments_count <- group2assigned_redistribution[[2]]
  
} else {
  group2assigned_redistributed <- group2assigned # Rename this matrix so that it can be called correctly later
}

table(inputdfroundtwo_assignments_count)
# If there is more than one NA in any row (an application does not have at least 9 reviewers)
# Then take an assignee from a reviewer that does have 10, ensuring that it is a valid swap
# And if people haven't been assigned 10 reviews, swap with people who have 10 until everybody has at least 9
if(any(apply(group2assigned_redistributed, 1, function(row) sum(is.na(row)) > 1))){
  
  group2assigned_redistribution_subten_assignees <- redistribute_sub_ten_assignees(assigned_group_redistributed = group2assigned_redistributed, constraint_list = group_two_constraint_list) 
  group2assigned_redistributed <- group2assigned_redistribution_subten_assignees
} else if(any(table(inputdfroundone_assignments_count)<9)){
  group2assigned_redistribution_subten <- redistribute_sub_ten_assignments(assigned_group_redistributed = group2assigned_redistributed,
                                                                           constraint_list = group_two_constraint_list)
  group2assigned_redistributed <- group2assigned_redistribution_subten[[1]]
  inputdfroundone_assignments_count <- as.numeric(table(group2assigned_redistributed))
}

table(inputdfroundone_assignments_count, useNA="always")
# Combine the two allocation matrices
combined_groups <- rbind(group1assigned_redistributed, group2assigned_redistributed)

# Check that each application is reviewing 9 or more times
all(table(combined_groups) >8)

which(table(combined_groups)<10)

# Number of applications with less than 10 reviewers
length(which(apply(combined_groups, 1, function(row) sum(is.na(row)) > 0)))

# Create a final dataframe, each reviewer is a row and reviewees are columns
finaldf <- as.data.frame(combined_groups, row.names=rownames(combined_groups)) |>
  mutate(ApplicantName = as.numeric(row.names(combined_groups)), .before=everything()) |>
  rename_with( ~ paste0("Reviewer", 1:ncol(combined_groups)), .cols = 2:(ncol(combined_groups)+1)) |>
  mutate(Application = ApplicantName, .before = everything()) |>
  interleave_columns() |>
  mutate(across(starts_with("Reviewer") | starts_with("ApplicantName"), translate_from_ID_to_name)) |>
  mutate(across(starts_with("Email"), translate_from_ID_to_email)) |>
  mutate(Application = translate_from_ID_to_ApplicationNumber(Application))
  
# table(combined_groups)
# 
# # Create excel file where each sheet is a person and their allocations
# write_xlsx(finaldf, "results/fullDPRallocation280425.xlsx")


