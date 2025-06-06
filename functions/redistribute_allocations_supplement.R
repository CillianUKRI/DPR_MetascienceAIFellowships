# Function to check if an assignment is valid
is_valid_assignment <- function(row, assignedperson, constraint_list, assigned_group) {
  row_constraints <- assigned_group[row, ]
  combined_constraints <- unique(c(constraint_list[[row]], row_constraints))
  !any(assignedperson %in% combined_constraints)
}

# Count the number of times each person is assigned
assignment_counts <- table(group1assigned_redistributed)

# Identify under-assigned and over-assigned people, shuffled
under_assigned <- sample(as.numeric(names(assignment_counts[assignment_counts < 9])))
over_assigned <- sample(as.numeric(names(assignment_counts[assignment_counts > 9])))

# Reassign people
for (person in under_assigned) {
  while (assignment_counts[as.character(person)] < 9) {
    reassigned <- FALSE
    for (over_person in over_assigned) {
      if (assignment_counts[as.character(over_person)] > 9) {
        # Find a valid reassignment
        for (i in 1:nrow(group1assigned_redistributed)) {
          for (j in 1:ncol(group1assigned_redistributed)) {
            if (group1assigned_redistributed[i, j] == over_person && is_valid_assignment(i, person, group_one_constraint_list, group1assigned_redistributed)) {
              group1assigned_redistributed[i, j] <- person
              assignment_counts[as.character(person)] <- assignment_counts[as.character(person)] + 1
              assignment_counts[as.character(over_person)] <- assignment_counts[as.character(over_person)] - 1
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

# Verify the new assignment counts
assignment_counts <- table(group1assigned_redistributed)
print(assignment_counts)


# Identify rows with more than one NA value
rows_with_multiple_NAs <- apply(group2assigned_redistributed, 1, function(row) sum(is.na(row)) > 1)
rows_with_multiple_NAs_indices <- which(rows_with_multiple_NAs)

# Redistribution logic
for (i in rows_with_multiple_NAs_indices) {
  na_indices <- which(is.na(group2assigned_redistributed[i, ]))
  
  for (na_index in na_indices) {
    for (j in sample(1:nrow(group2assigned_redistributed))) {
      if (i != j && sum(is.na(group2assigned_redistributed[j, ])) == 0) {
        for (k in 1:ncol(group2assigned_redistributed)) {
          if (!is.na(group2assigned_redistributed[j, k]) &&
              is_valid_assignment(i, group2assigned_redistributed[j, k], group_two_constraint_list, group2assigned_redistributed)) {
            
            # Swap the values
            group2assigned_redistributed[i, na_index] <- group2assigned_redistributed[j, k]
            group2assigned_redistributed[j, k] <- NA
            
            # Update the list of rows with multiple NAs
            rows_with_multiple_NAs <- apply(group2assigned_redistributed, 1, function(row) sum(is.na(row)) > 1)
            rows_with_multiple_NAs_indices <- which(rows_with_multiple_NAs)
            
            break
          }
        }
      }
      if (!is.na(group2assigned_redistributed[i, na_index])) break
    }
  }
}


# 
# if(all(table(group2assigned_redistributed)==10) & any(is.na(group2assigned_redistributed[,]))){
#   reviewers_lessthan_ten_assignees <- apply(group2assigned_redistributed, 1, function(row) sum(is.na(row)) > 1)
#   reviewers_lessthan_ten_assignees_index <- as.numeric(rownames(group2assigned_redistributed)[which(reviewers_lessthan_ten_assignees)])
#   
#   print("IF A")
#   
#   for(person in reviewers_lessthan_ten_assignees_index){
#     print("Loop A")
#     while(sum(is.na(group2assigned_redistributed[as.character(person),])) > 1){
#       reviewers_with_ten <- setdiff(as.numeric(rownames(group2assigned_redistributed)),reviewers_lessthan_ten_assignees_index)
#       reassigned <- FALSE
#       
#       for(fullperson in reviewers_with_ten){
#         print("Loop B")
#         if(sum(is.na(group2assigned_redistributed[as.character(person),])) > 1){
#           for (j in 1:ncol(group2assigned_redistributed)) {
#             if (!is.na(group2assigned_redistributed[as.character(fullperson), j]) && is_valid_assignment(row = as.character(person), assignedperson = group2assigned_redistributed[as.character(fullperson),j], group_two_constraint_list, group2assigned_redistributed)
#                 && sum(is.na(group2assigned_redistributed[as.character(fullperson),])) == 0) {
#               group2assigned[as.character(person), which(is.na(group2assigned_redistributed[as.character(person),]))[1]] <- group2assigned_redistributed[as.character(fullperson),j]
#               group2assigned_redistributed[as.character(fullperson),j] <- NA
#               
#               reviewers_lessthan_ten_assignees <- apply(group2assigned_redistributed, 1, function(row) sum(is.na(row)) > 1)
#               reviewers_lessthan_ten_assignees_index <- as.numeric(rownames(group2assigned_redistributed)[which(reviewers_lessthan_ten_assignees)])
#               print("Internal IF")
#               
#               reassigned <- TRUE
#               break
#             }
#           }
#         }
#         if (reassigned) break
#       }
#       if (!reassigned) break
#     }
#   }
# }