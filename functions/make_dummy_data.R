set.seed(222) # For reproducibility

##### Make dummy data #####

size_of_inputdf <- 333

#Generate 100 unique names
firstnames <- c("Tom", "Jim", "Lisa", "Mary", "Xavier", "Hugh", "John", "Ben", "Lucy", "Naomi", "Sophie", "Ed",
                "Colin", "Trevor", "Larry", "Fiona", "James", "Susan", "Jeffrey", "Amy", "Cormac", "Aoife", "Fintan")
surnames <- c("Murray", "Johnson", "MacDonald", "Brooke", "Bradley", "Love", "Olsen", "Paul", "Winston", 
              "Luther", "Smith", "Young", "Carr", "Healy", "McNulty", "Gallagher", "Culhane", "Halpin", "Reilly")

fullname <- rep("Blank", size_of_inputdf)

# Generate full names from the list of firstnames and surnames
# Only unique names are generated
for (name in 1:length(fullname)) {
  repeat {
    # Generate a new full name
    new_name <- paste(sample(firstnames, 1), sample(surnames, 1))
    
    # Check if the name is already in the list
    if (!new_name %in% fullname) {
      fullname[name] <- new_name
      break # Exit the repeat loop when a unique name is found
    }
  }
}

#Generate department and institution
department <- c("Biochemistry", "Philosophy", "Economics", "Italian", "Psychology", "History", "Health", "Business and Law",
                "Engineering")

institution <- c("Uni of Oxford", "Uni of Nottingham", "Manchester Uni", "Cambridge", "LSE", "Imperial",
                 "Cardiff", "UCL")


# Assign departments and institution to each person
# Randomly assigned from the department and institution vectors
detaildf <- data.frame(fullname=fullname,
                       department=sample(department, size=size_of_inputdf, replace=T, prob=rep(0.1,9)),
                       institution=sample(institution,size=size_of_inputdf, replace=T, prob=rep(0.1,8)),
                       Conflictnames = "BLANK")

# #Add potential conflicts for each person
# This section can be uncommented to add conflicts to each person
# for(i in 1:length(detaildf$fullname)){
#   
#   numberofconflicts <- sample(c(0:10),1)
#   
#   namesother <- detaildf$fullname[-i]
#   
#   if(numberofconflicts > 0){
#     detaildf$Conflictnames[i] <- paste(strsplit(sample(namesother,numberofconflicts), "  "), collapse = ", ")
#   } else {
#     detaildf$Conflictnames[i] <- "None"
#   }
#   
# }

# inputdf <- detaildf |>
#   setNames(c("Name", "Department", "Institution", "Conflicts")) |>
#   select(Name, Department, Institution, Conflicts) 

inputdf <- detaildf |>
  setNames(c("ApplicantName", "Department", "ApplicantOrganisation", "Conflicts")) |>
  select(ApplicantName, Department, ApplicantOrganisation) |>
  mutate(Conflicts = "None")

inputdf$ID <- seq(1:nrow(inputdf))

# Randomly split pool into two
inputdfrandomsample <- sample(inputdf$ID, floor(nrow(inputdf)/2), replace=FALSE)
inputdfroundone <- inputdf$ID[inputdf$ID %in% inputdfrandomsample]
inputdfroundtwo <- inputdf$ID[!(inputdf$ID %in% inputdfrandomsample)]

# Create tables used for "translating" between indices
inputdfroundtwo_assignments_countdf <- data.frame(count = rep(0, length(inputdfroundtwo)),
                                                  groupindex = inputdfroundtwo,
                                                  rownumber = seq(1, length(inputdfroundtwo))) # Track how many times each person in Group 2 is assigned

inputdfroundone_assignments_countdf <- data.frame(count = rep(0, length(inputdfroundone)),
                                                  groupindex = inputdfroundone,
                                                  rownumber = seq(1, length(inputdfroundone))) # Track how many times each person in Group 2 is assigned



# Create count vectors
inputdfroundtwo_assignments_count <- rep(0, length(inputdfroundtwo))
inputdfroundone_assignments_count <- rep(0, length(inputdfroundone))
