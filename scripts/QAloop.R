#################################################################################################
# Author: Cillian Brophy
# Date last modified: 14/04/25
# Script purpose: This script is used to loop the DPR allocation program ("main.R") until a QA error occurs (found in "qualityassurance.R").
#                 A QA error occurs if: 
#                 any applicant is given a reviewer from the same group, or
#                 any applicant is given a reviewer that they shouldn't be given based on conflicts (constraints), or
#                 any applicant is given the same reviewer more than once.
# Workflow and file dependencies: This script is only run for testing the program.
#################################################################################################

QA <- TRUE
x <- 1

while(QA == TRUE){
  
  print(paste0("Iteration ", x, " starting."))
  source("scripts/main.R")
  source("scripts/qualityassurance.R")
  
  
  
  x <- x+1
  
}
