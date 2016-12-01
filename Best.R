best <- function(which_state, outcome){
  # Read the outcome data into R via the read.csv function and look at the first few rows.
  hospdb1 <- read.csv("outcome-of-care-measures.csv",
                                na.strings = "Not Available", stringsAsFactors=FALSE)
  
  ## Check that state and outcome are valid
  check_state <- hospdb1[,7]
  if (!which_state %in% check_state) stop("Invalid state")
  hospdb2 <- subset(hospdb1, select=c(2,7,11,17,23))
  colnames(hospdb2)<-c("Hospital","State","heart attack","heart failure","pneumonia")
  check_outcome <-c(colnames(hospdb2))
  if (!outcome %in% check_outcome) stop("Invalid outcome")

  ## Return hospital name in that state with lowest 30-day death rate
  column_names <- c("Hospital","State",outcome)
  hospdbfinal <- subset(hospdb2,State == which_state,select=c(column_names))
  summary(hospdbfinal)
  hospital_name <- subset(hospdbfinal,
    hospdbfinal[,3]==min(hospdbfinal[,3],na.rm = TRUE),1)
  hospital_name
}

# Testing invalid responses
best("BB", "heart attack") #run this code
# results in "Error in best("BB", "heart attack") : invalid state"
best("NY", "hert attack") #run this code
# results in "Error in best("NY", "hert attack") : invalid outcome"

# Testing VALID responses
best("TX", "heart attack") #result is "CYPRESS FAIRBANKS MEDICAL CENTER"
best("TX", "heart failure") # result is ""FORT DUNCAN MEDICAL CENTER""
best("MD", "heart attack") # result is "JOHNS HOPKINS HOSPITAL, THE"
best("MD", "pneumonia") # result is "GREATER BALTIMORE MEDICAL CENTER"

