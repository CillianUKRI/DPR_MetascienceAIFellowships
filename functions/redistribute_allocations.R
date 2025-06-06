#################################################################################################
# Author: Cillian Brophy
# Date last modified: 14/04/25
# Script purpose: This script contains three functions.
#                 groupallocations_redistribute() is used straight after DPR_allocation() and checks if reviewers can be swapped between applicants
#                 in order to give reviewers to the last few applicants that might have not had any eligible matches available due to DPR_allocation() running from the top of the applicant list to the bottom.
#
#                 redistribute_sub_ten_assignments() looks at how many times each applicant has been assigned as a reviewers.
#                 If any reviewer is reviewing less than 9 times, it tries to swap reviewers until this number is reached.
#
#                 redistribute_sub_ten_assignees() works in much the same way as groupallocations_redistribute() but acts as a final check.
#
# Workflow and file dependencies: This script is loaded through main.R.
#################################################################################################


# Function to be used after DPR_Allocation - Will iterate through the list of people again and
# swap any possible reviewers from one applicant to one of the unfilled applicants if possible
# It works by looking at each applicant who has 10 reviewers, checking if any of those 10 could go to one of the
# Unfilled applicants, and if so could a reviewer from the available pool be swapped instead?


groupallocations_redistribute <- function(group_to_allocate, group_allocations_from, 
                                          assigned_group, constraint_list,
                                          count_of_assigned, translation_df) {
  
  # Cycle through each applicant in the group to be assigned who doesn't have 10 reviewers yet
  for(i in reviewers_lessthan_ten_assignees){
    forbidden_greed <- c(assigned_group[i,], constraint_list[[i]], group_to_allocate) # The list of forbidden people for the person who doesn't have 10 reviewers yet
    
    # Now cycle through everybody in the group that is receiving allocations and see if a swap can take place
    for(person in group_to_allocate){
      forbidden <- c(assigned_group[as.character(person),], constraint_list[[as.character(person)]])
      
      # Get available people in the group where allocations are from (those with fewer than 10 assignments)
      available_in_group_of_assignees <- group_allocations_from[count_of_assigned < 10]
      available_in_group_of_assignees <- na.omit(setdiff(available_in_group_of_assignees, forbidden))
      
      forbidden_greed <- c(assigned_group[i,], constraint_list[[i]], group_to_allocate)
      
      if((length(which(is.na(assigned_group[i,]))) > 0) & # If there are NAs to be filled
         (length(available_in_group_of_assignees) > 0) & # And there are eligible assignees already assigned to this person
         (length(na.omit(setdiff(assigned_group[as.character(person),], forbidden_greed))) > 0 )){ # And one of these eligible people would be eligible for the person with less than 10 assignees
        
        index_for_swappingNA <- which(is.na(assigned_group[i,]))[1] # Index for the first NA to be filed
        
        # From the person who already has 10, take a random assignee who is not forbidden for the new person (duplicates or conflicts)
        # Use an if statement here because if available_in_group_of_assignees only has 1 element, R will sample 1:element
        if(length(na.omit(setdiff(assigned_group[as.character(person),], forbidden_greed))) > 1){
          move_from_assigned <- sample(na.omit(setdiff(assigned_group[as.character(person),], forbidden_greed)), 1)
        } else {
          move_from_assigned <- na.omit(setdiff(assigned_group[as.character(person),], forbidden_greed))
        } 
        
        # Use an if statement here because if available_in_group_of_assignees only has 1 element, R will sample 1:element
        if(length(available_in_group_of_assignees) > 1){
          swapped_extra_assignment <- sample(available_in_group_of_assignees, 1)
        } else {
          swapped_extra_assignment <- available_in_group_of_assignees
        }
        
        
        index_for_swapping_assigned <- which(assigned_group[as.character(person),] == move_from_assigned)
        assigned_group[as.character(person), index_for_swapping_assigned] <- swapped_extra_assignment
        assigned_group[i,index_for_swappingNA] <- move_from_assigned
        
        
        selected_assignments <- swapped_extra_assignment
        
        convertassignments <- convertindices(translation_df, selected_assignments, toID=TRUE)
        count_of_assigned[convertassignments] <- count_of_assigned[convertassignments] + 1 
        
        print(paste0("Person ", as.character(person), " is giving ", move_from_assigned, " to Person ", rownames(assigned_group)[i],
                     ". Person ",  as.character(person), " is receiving ", swapped_extra_assignment, " from the available pool as replacement."))
        
        
      }
    } 
  }
  return(list(assigned_group = assigned_group, countvector = count_of_assigned))
}

# This function comes into play when there are reviewers who have been assigned less than 9 times.
# It swaps reviewers from applicants with 10 reviewers to those with less than 9, until everybody has at least 9 or 10.
# It checks if swaps are valid first and only does valid swaps.
redistribute_sub_ten_assignments <- function(assigned_group_redistributed, constraint_list) {
  
  count_of_assigned <- table(assigned_group_redistributed)
  # Identify under-assigned and over-assigned people, shuffled
  if(length(as.numeric(names(count_of_assigned[count_of_assigned < 9]))) == 1){
    under_assigned <- as.numeric(names(count_of_assigned[count_of_assigned < 9]))
  } else {
  under_assigned <- sample(as.numeric(names(count_of_assigned[count_of_assigned < 9])))
  }
  
  if(length(as.numeric(names(count_of_assigned[count_of_assigned > 9]))) == 1){
    over_assigned <- as.numeric(names(count_of_assigned[count_of_assigned > 9]))
  } else{
  over_assigned <- sample(as.numeric(names(count_of_assigned[count_of_assigned > 9])))
  }
  
  # Reassign people
  for (person in under_assigned) { # Iterate over every person who has less than 9 assignees
    while (count_of_assigned[as.character(person)] < 9) { # Loop only while this person has less than 9 assignees
      reassigned <- FALSE
      for (over_person in over_assigned) { # Iterate over every person who has more than 9 assignees
        if (count_of_assigned[as.character(over_person)] > 9) { 
          # Search every single value in the matrix one at a time. If the value belongs to someone with more than 9 assignees, and would be a valid match for the under-assigned person, swap it.
          for (i in 1:nrow(assigned_group_redistributed)) { 
            for (j in 1:ncol(assigned_group_redistributed)) {
              if (assigned_group_redistributed[i, j] == over_person && is_valid_assignment(i, person, constraint_list, assigned_group_redistributed)) {
                assigned_group_redistributed[i, j] <- person
                count_of_assigned[as.character(person)] <- count_of_assigned[as.character(person)] + 1
                count_of_assigned[as.character(over_person)] <- count_of_assigned[as.character(over_person)] - 1
                reassigned <- TRUE
                break
              }
            }
            if (reassigned) break
          }
        }
        if (reassigned) break
      }
      if (!reassigned) break  # Exit the while loop if no reassignment was possible
    }
  }
  return(list(assigned_group_redistributed = assigned_group_redistributed, countvector = count_of_assigned))
}



# This function finds which applicants have more than one "NA" (missing value). So they have 8 or less reviewers.
# It then identifies applicants who have the full 10 complement of reviewers and checks if one of these can be swapped to the applicant with 8 or less.
redistribute_sub_ten_assignees <- function(assigned_group_redistributed, constraint_list){
  # Identify rows with more than one NA value
  rows_with_multiple_NAs <- apply(assigned_group_redistributed, 1, function(row) sum(is.na(row)) > 1)
  rows_with_multiple_NAs_indices <- which(rows_with_multiple_NAs)
  
  # Redistribution logic
  for (i in rows_with_multiple_NAs_indices) {
    na_indices <- which(is.na(assigned_group_redistributed[i, ]))
    
    for (na_index in na_indices) {
      for (j in sample(1:nrow(assigned_group_redistributed))) {
        if (i != j && sum(is.na(assigned_group_redistributed[j, ])) == 0) {
          for (k in 1:ncol(assigned_group_redistributed)) {
            if (!is.na(assigned_group_redistributed[j, k]) &&
                is_valid_assignment(i, assigned_group_redistributed[j, k], constraint_list, assigned_group_redistributed)) {
              
              # Swap the values
              assigned_group_redistributed[i, na_index] <- assigned_group_redistributed[j, k]
              assigned_group_redistributed[j, k] <- NA
              
              # Update the list of rows with multiple NAs
              rows_with_multiple_NAs <- apply(assigned_group_redistributed, 1, function(row) sum(is.na(row)) > 1)
              rows_with_multiple_NAs_indices <- which(rows_with_multiple_NAs)
              
              break
            }
          }
        }
        if (!is.na(assigned_group_redistributed[i, na_index])) break
      }
    }
  }
  return(assigned_group_redistributed)
}