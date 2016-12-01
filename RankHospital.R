# TITLE:- Ranking hospitals by outcome in a state
# Write a function called rankhospital that takes three arguments: the 2-character 
# abbreviated name of a state (state), an outcome (outcome), and the ranking of a hospital
# in that state for that outcome (num). The function reads the outcome-of-care-measures.csv 
# file and returns a character vector with the name of the hospital that has the ranking
# specified by the num argument. For example, the call 
# rankhospital("MD", "heart failure", 5) would return a character vector containing the
# name of the hospital with the 5th lowest 30-day death rate for heart failure. 
# The num argument can take values "best", "worst", or an integer indicating the ranking
# (smaller numbers are better). If the number given by num is larger than the number of
# hospitals in that state, then the function should return NA. Hospitals that do not have
# data on a particular outcome should be excluded from the set of hospitals when deciding
# the rankings.Ties should be broken by using the hospital name sorted ascending.



rankhospital <- function(which_state, outcome, num = "best") {
  # Read the outcome data into R via the read.csv function.
  hospdb1 <- read.csv("outcome-of-care-measures.csv",
                                na.strings = "Not Available", stringsAsFactors=FALSE)
  
  ## Check that state and outcome are valid
  check_state <- hospdb1[,7]
  if (!which_state %in% check_state) stop("Invalid state")
  hospdb2 <- subset(hospdb1, select=c(2,7,11,17,23))
  colnames(hospdb2)<-c("Hospital","State","heart attack","heart failure","pneumonia")
  check_outcome <-c(colnames(hospdb2))
  if (!outcome %in% check_outcome) stop("Invalid outcome")
  
  # preparing the database
  column_names <- c("Hospital","State",outcome)
  hospdb3 <- subset(hospdb2,State == which_state,select=c(column_names))
  hospdbfinal <- hospdb3[hospdb3[,3]!="Not Available",]
  hospdbfinal <- hospdbfinal[order(hospdbfinal[,3],hospdbfinal[,1]),]
  hospdbfinal <- hospdbfinal[complete.cases(hospdbfinal),]
  
  
  ## Return hospital name in that state with the given rank 30-day death rate    
  if (num =="best" | num ==1){hospdbfinal[1,1]}
  else if (num == "worst"){hospdbfinal[nrow(hospdbfinal),1]}
  else if (num > length(hospdbfinal[,1])){stop("NA")} 
  else hospdbfinal[num,1]
}

# Testing responses
rankhospital("TX", "heart failure", 4) # results in "DETAR HOSPITAL NAVARRO"
rankhospital("MD", "heart attack", "worst") # results in "HARFORD MEMORIAL HOSPITAL"
rankhospital("MN", "heart attack", 5000) # results in "NA"

