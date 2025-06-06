#################################################################################################
# Author: Cillian Brophy
# Date last modified: 14/04/25
# Script purpose: This script is used to perform various "sanity checks" on the output of the DPR allocation program ("main.R").
# Workflow and file dependencies: Run script from start to finish. Additional checks are commented out at the end.
#################################################################################################


# Create a dataframe from the final matrix, and add blank quality assurance columns
finaldf <- as.data.frame(combined_groups, row.names=rownames(combined_groups)) |>
  mutate(Application = as.numeric(row.names(combined_groups)), .before=everything()) |>
  rename_with( ~ paste0("Reviewer", 1:ncol(combined_groups)), .cols = 2:(ncol(combined_groups)+1)) |>
  mutate(AllRevieweesInOtherGroup = "BLANK") |>
  mutate(AnyConstraints = "BLANK")

# This populates the AllRevieweesInOtherGroup column which will specify if all reviewers are in the different group than the applicant (they should be)
for(i in 1:nrow(finaldf)){
  if(finaldf[i,1] %in% inputdfroundone){
    groupnumber <- "One"
  } else {
    groupnumber <- "Two"
  }
  
  allassignees <- as.numeric(finaldf[i,2:11])
  
  if((all(allassignees %in% inputdfroundone) & groupnumber == "Two") | (all(allassignees %in% inputdfroundtwo) & groupnumber == "One")){
    finaldf$AllRevieweesInOtherGroup[i] <- "All in other group"
  }  else if ((any(allassignees %in% inputdfroundone) & groupnumber == "One") | (any(allassignees %in% inputdfroundtwo) & groupnumber == "Two")){
    finaldf$AllRevieweesInOtherGroup[i] <- "At least one is in same group" 
  } else if((all(na.omit(allassignees) %in% inputdfroundone) & groupnumber == "Two" & sum(is.na(allassignees)>0)) | (all(na.omit(allassignees) %in% inputdfroundtwo) & groupnumber == "One" & sum(is.na(allassignees)>0))){
    finaldf$AllRevieweesInOtherGroup[i] <- "All in other group and at least one NA"
  } else {
    finaldf$AllRevieweesInOtherGroup[i] <- "ERROR"
  }
}

# Bind the two constraint lists together to form a master
fullconstraintlist <- c(group_one_constraint_list, group_two_constraint_list)

# This populates the AnyConstraints column which will flag if any Reviewer is a constraint of the Applicant (there should be no constraints)
for(i in rownames(finaldf)){
  
  allassignees <- as.numeric(finaldf[as.character(i), 2:11])
  
  if(any(allassignees %in% fullconstraintlist[[as.character(i)]]) && sum(is.na(finaldf[i,])) == 0){
    finaldf[as.character(i),]$AnyConstraints <- "There is a constraint and no NAs"
  } else if (all(!allassignees %in% fullconstraintlist[[as.character(i)]]) && sum(is.na(finaldf[i,])) == 0) {
    finaldf[as.character(i),]$AnyConstraints <- "No constraints and no NAs"
  } else if ((sum(is.na(finaldf[i,])) == 1) && (any(allassignees %in% fullconstraintlist[[as.character(i)]]))){
    finaldf[as.character(i),]$AnyConstraints <- "There are constraints and one NA"
  } else if ((sum(is.na(finaldf[i,])) == 1) && (!any(allassignees %in% fullconstraintlist[[as.character(i)]]))){
    finaldf[as.character(i),]$AnyConstraints <- "No constraints and one NA"
  } else {
    finaldf[as.character(i),]$AnyConstraints <- "ERROR"
  }
}

# Check that each application has unique reviewers
finaldf <- finaldf |>
  rowwise() %>%
  mutate(unique_values = length(unique(na.omit(c_across(2:11)))) == sum(!is.na(c_across(2:11)))) %>%
  ungroup()

table(finaldf$AnyConstraints)
table(finaldf$AllRevieweesInOtherGroup)

# Check that the reciprocals were correctly added as conflicts (constraints) to group 2. Currently not working for even-numbered input tables

constraintslogic <- all(unique(finaldf$AnyConstraints) %in% c("No constraints and no NAs", "No constraints and one NA")) && length(unique(finaldf$AnyConstraints)) == 2 
grouplogic <- all(unique(finaldf$AllRevieweesInOtherGroup) %in% c("All in other group", "All in other group and at least one NA")) && length(unique(finaldf$AllRevieweesInOtherGroup)) == 2
uniquereviewers <- all(finaldf$unique_values == T)

# These boolean columns are used in the QAloop.R file.
# if(constraintslogic == F){
#   QA <- FALSE
# }
# 
# if(grouplogic==F){
#   QA <- FALSE
# }
# 
# if(uniquereviewers == F){
#   QA <- FALSE
# }

# Which applications have less than 10 reviewers
# which(apply(combined_groups, 1, function(row) sum(is.na(row)) > 0))

# Number of applications with less than 10 reviewers
# length(which(apply(combined_groups, 1, function(row) sum(is.na(row)) > 0)))

# #Misc QA code
# #Find all applications that Marzia is reviewing
# QAdf <- finaldf |>
#   filter(if_any(starts_with("Reviewer"), ~ . == "Marzia Briel-Barr"))
# 
# #Which applications don't have the full 10
# which(apply(combined_groups, 1, function(row) sum(is.na(row)) > 0))
# # Check that all have more than 8 reviewers
# all(table(group1assigned_redistributed) >8)
# # Which IDs have less than 10 reviewers
# which(table(combined_groups) <10)
# 
# # Which people are reviewing less than 10 times
# translate_from_ID_to_name(as.numeric(names(which(table(combined_groups) <10))))
# # Which applications have less than 10 reviewers
# translate_from_ID_to_name(as.numeric(names(which(apply(combined_groups, 1, function(row) sum(is.na(row)) > 0)))))

# Add group number to inputdf
# inputdf2 <- inputdf |>
#   mutate(Group = case_when(ID %in% inputdfroundone_assignments_countdf$groupindex ~ "Group 1",
#                            ID %in% inputdfroundtwo_assignments_countdf$groupindex ~ "Group 2",
#                            TRUE ~ "ERROR")) |>
#   relocate(Group, .before = ApplicantName)


## Code to add a column, showing if there is a reciprocal match (there shouldn't be. Should all be FALSE)
# input df needs to only be the application and reviewer columns
# check_owner_values <- function(df) {
#   long_df <- df %>%
#     pivot_longer(cols = starts_with("Reviewer"), names_to = "Reviewer", values_to = "ReviewerValue")
#   
#   result <- long_df %>%
#     group_by(Application) %>%
#     summarize(HasMatchingOwner = any(sapply(ReviewerValue, function(owner) {
#       any(long_df %>% filter(Application == owner) %>% pull(ReviewerValue) == Application)
#     })))
#   
#   df <- df %>%
#     left_join(result, by = "Application")
#   
#   return(df)
# }
# # Apply the function to the dataframe
# result <- check_owner_values(bla)
