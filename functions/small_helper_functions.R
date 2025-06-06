#################################################################################################
# Author: Cillian Brophy
# Date last modified: 14/04/25
# Script purpose: This script contains a few different small custom functions used in the running of main.R (DPR allocation program).
#                 Each custom function has comments explaining its purpose.
# Workflow and file dependencies: This script is loaded through main.R.
#################################################################################################

# Convert index value to ID of person and vice versa
# (rownumbers get mixed up when splitting the group in two)
convertindices <- function(conversiondf, thingtoconvert, toID = TRUE){
  translatedvector <- c()
  
  if(toID == TRUE){
    for(number in thingtoconvert){
      tempindex <- which(conversiondf$groupindex==number)
      translatedvector <- c(translatedvector, tempindex)
    }
  } else if (toID == FALSE){
    for(number in thingtoconvert){
      tempindex <- conversiondf$groupindex[which(conversiondf$rownumber==number)]
      translatedvector <- c(translatedvector, tempindex)
    }
  }
  return(translatedvector)
}

# Assign reciprocals to group 2 for one person (used in find_all_reciprocals_in_group())
find_reciprocal_one_person <- function(matrix, number) {
  index_of_number <- which(apply(matrix, 1, function(row) number %in% row))
  as.numeric(rownames(matrix)[index_of_number])
}

# Function to work backwards from index number to names, to show which people are reciprocals
find_all_reciprocals_in_group <- function(matrix, IDlist) {
  result <- lapply(IDlist, function(num) find_reciprocal_one_person(matrix, num))
  names(result) <- IDlist
  return(result)
}

# Make a dataframe for each applicant, with their list of 10 reviewers
create_final_dataframes <- function(matrix, mastertable){
  df_list <- lapply(1:nrow(matrix), function(i) {
    row_numbers <- matrix[as.character(i),]
    data.frame(name = mastertable$Name[row_numbers],
               department = mastertable$Department[row_numbers],
               institution = mastertable$Institution[row_numbers])
  })
  return(df_list)
} 


# Translate the raw ID value to a name/email/AppNumber (used when generating the final dataframe)
# Converts IDs to names/email/AppNumber, ignores any NA values to stop R converting the output to lists
translate_from_ID_to_name  <- function(x){
  sapply(x, function(i) {
    if (is.na(i)) {
      return(NA)
    } else {
      row_from_ID <- which(inputdf$ID == i)
      name_from_index <- ifelse(length(row_from_ID) > 0, inputdf$ApplicantName[row_from_ID], NA)
      return(name_from_index)
    }
  }, USE.NAMES = FALSE)
}

translate_from_ID_to_email  <- function(x){
  sapply(x, function(i) {
    if (is.na(i)) {
      return(NA)
    } else {
      row_from_ID <- which(inputdf$ID == i)
      name_from_index <- ifelse(length(row_from_ID) > 0, inputdf$ApplicantEmail[row_from_ID], NA)
      return(name_from_index)
    }
  }, USE.NAMES = FALSE)
}

translate_from_ID_to_ApplicationNumber  <- function(x){
  sapply(x, function(i) {
    if (is.na(i)) {
      return(NA)
    } else {
      row_from_ID <- which(inputdf$ID == i)
      name_from_index <- ifelse(length(row_from_ID) > 0, inputdf$Reference[row_from_ID], NA)
      return(name_from_index)
    }
  }, USE.NAMES = FALSE)
}

# Add in Email columns after each Reviewer column in the final dataframe that is output.
interleave_columns <- function(df) {
  reviewer_cols <- grep("^Reviewer", names(df), value = TRUE)
  new_cols <- map2(reviewer_cols, seq_along(reviewer_cols), ~ c(.x, paste0("EmailReviewer", .y))) %>% 
    unlist()
  
  df %>%
    mutate(across(all_of(reviewer_cols), ~ ., .names = "Email{col}")) %>%
    select(Application, ApplicantName, all_of(new_cols))
  
}

# Function to check if an assignment is valid
is_valid_assignment <- function(row, assignedperson, constraint_list, assigned_group) {
  row_constraints <- assigned_group[row, ]
  combined_constraints <- unique(c(constraint_list[[row]], row_constraints))
  !any(assignedperson %in% combined_constraints)
}