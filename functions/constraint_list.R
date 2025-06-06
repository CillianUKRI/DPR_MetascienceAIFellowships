#################################################################################################
# Author: Cillian Brophy
# Date last modified: 14/04/25
# Script purpose: This script generates a list of forbidden matches for each applicant.
#                 Forbidden matches include same name, 
# Workflow and file dependencies: This script is loaded through main.R
#################################################################################################


generate_constraint_list <- function(data1, data2, mastertable) {
  constraint_list <- vector("list", length(data1))
  names(constraint_list) <- as.character(data1)
  
  group_allocations_from <- mastertable[mastertable$ID %in% data2,]
  
  for(person in data1){
    row_index <- which(mastertable$ID==person)
    current_person <- mastertable[row_index,]
    
    forbidden_person <- c()
    forbidden_person <- c(forbidden_person, as.numeric(current_person$ID))
    
    for(i in 1:nrow(group_allocations_from)){
      candidate <- group_allocations_from[i,]
      
      
      
      if (candidate["ApplicantName"] == current_person$ApplicantName) {
        forbidden_person <- c(forbidden_person, as.numeric(candidate["ID"]))
      }
      
      if (candidate["ApplicantName"] %in% strsplit(current_person$Conflicts, ", ")[[1]]){
        forbidden_person <- c(forbidden_person, as.numeric(candidate["ID"]))
      }

      # if (candidate["ApplicantName"] %in% strsplit(current_person$`Co-Is`, ", ")[[1]]){
      #   forbidden_person <- c(forbidden_person, as.numeric(candidate["ID"]))
      # }
      
      if((candidate["ApplicantOrganisation"] == current_person$ApplicantOrganisation)) {
        forbidden_person <- c(forbidden_person, as.numeric(candidate["ID"]))
      }
      
    }
    
    if(length(forbidden_person) != 0){
      constraint_list[[as.character(person)]] <- forbidden_person
    } # Only modify list if there is a forbidden_person. Otherwise R will change the size of list.
  }
  return(constraint_list)
}

# If want to make a conflict for same institution AND department:
# if((candidate["Institution"] == current_person$Institution) & (candidate["Department"] == current_person$Department)) {
#   forbidden_person <- c(forbidden_person, as.numeric(candidate["ID"]))
# }

