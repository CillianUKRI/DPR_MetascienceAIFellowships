#################################################################################################
# Author: Cillian Brophy
# Date last modified: 14/04/25
# Script purpose: This script contains the code that is first used to allocate reviewers to applicants in DPR.
#                 It is run twice. Once for each of the two groups.
#                 It works by iteratively looking at each applicant in a group, generating a list of available reviewers (that have not been selected to review 10 times yet),
#                 removing any "forbidden matches" for that person from the available list, and randomly assigning 10 people from this pool to the applicant.
# Workflow and file dependencies: This script is loaded through main.R.
#################################################################################################


# Function to assign Group 1 members to Group 2 and vice versa
DPR_allocation <- function(group_to_allocate, group_allocations_from,
                           constraint_list, count_of_assigned, translation_df) {
  
  # Create assignment matrix for group
  assigned <- matrix(NA, nrow = length(group_to_allocate), ncol = 10, dimnames = list(group_to_allocate))
  
  # Cycle through each applicant in the group to allocate
  # Match people with reviewers in other group, while taking into account constraint list and while ensuring that 
  # no reviewer is assigned more than 10 times (count_of_assigned)
  for (i in group_to_allocate) {
    forbidden <- constraint_list[[as.character(i)]] # Extract the forbidden people for this person
    
    # Get available people in Group 2 (those with fewer than 10 reviewers)
    available_in_group_of_assignees <- group_allocations_from[count_of_assigned < 10] # Extract the people left to be assigned who haven't been assigned 10 times yet
    available_in_group_of_assignees <- na.omit(setdiff(available_in_group_of_assignees, forbidden))  # Remove forbidden pairings, excluding NAs
    
    # Ensure there are enough valid assignments
    # And if there are, assign 10 reviewers from available pool randomly to this person
    if (length(available_in_group_of_assignees) >= 10) {
      selected_assignments <- sample(available_in_group_of_assignees, 10) # Randomly assign 10 of the available pool
      assigned[as.character(i), ] <- selected_assignments
      convertassignments <- convertindices(translation_df, selected_assignments, toID=TRUE) # Use helper function to translate the index numbers to row numbers and vice versa
      count_of_assigned[convertassignments] <- count_of_assigned[convertassignments] + 1 
    } 
    
    # If there are not 10 valid assignments, then assign however many there are 
    # And print a statement showing that there were not enough
    else if (length(available_in_group_of_assignees) == 0){
      print(paste0("No valid assignments for person ", i, "."))
    } else {
      selected_assignments <- available_in_group_of_assignees
      assigned[as.character(i), 1:length(selected_assignments)] <- selected_assignments
      convertassignments <- convertindices(translation_df, selected_assignments, toID=TRUE)
      count_of_assigned[convertassignments] <- count_of_assigned[convertassignments] + 1 
      print(paste0("Not enough valid assignments left for person ", i, "."))
    }
  }
  return(list(assigned = assigned, countvector = count_of_assigned))
}