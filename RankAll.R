# TITLE:- Ranking hospitals in all states

# Write a function called rankall that takes two arguments: an outcome name (outcome) and
# a hospital ranking (num). The function reads the outcome-of-care-measures.csv file and
# returns a 2-column data frame containing the hospital in each state that has the ranking
# specified in num. For example the function call rankall("heart attack", "best") would 
# return a data frame containing the names of the hospitals that are the best in their
# respective states for 30-day heart attack death rates. The function should return a 
# value for every state (some may be NA). The first column in the data frame is named
# hospital, which contains the hospital name, and the second column is named state, which
# contains the 2-character abbreviation for the state name. Hospitals that do not have
# data on a particular outcome should be excluded from the set of hospitals when deciding
# the rankings. The rankall function should handle ties in the 30-day mortality rates in
# the same way that the rankhospital function handles ties.

rankall <- function(outcome, num = "best"){
  ## Read outcome data
  hospdb1 <- read.csv("outcome-of-care-measures.csv",
                      na.strings = "Not Available", stringsAsFactors=FALSE)
  
  ## Check that state and outcome are valid
  hospdb2 <- subset(hospdb1, select=c(2,7,11,17,23))
  colnames(hospdb2)<-c("Hospital","State","heart attack","heart failure","pneumonia")
  check_outcome <-c(colnames(hospdb2))
  if (!outcome %in% check_outcome) stop("Invalid outcome")
  
  # preparing the database
  column_names <- c("Hospital","State",outcome)
  hospdb2 <- subset(hospdb2,select=c(column_names))
  hospdb2 <- hospdb2[hospdb2[,3]!="Not Available",]
  hospdb2 <- hospdb2[order(hospdb2[,2],hospdb2[,3],hospdb2[,1]),]
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  hospdbfinal <- aggregate(hospdb2,by=list(hospdb2[,2]),function(z){
    if(!is.numeric(num)){
      if(num =="best"){num <-1}
      else if (num =="worst"){num <-length(z)}
      else {stop("invalid outcome - please enter best, worst, or a number")}
    }
    z[num]
  })
  hospdbfinal[,c(2,3)]
}

# Testing responses - refer to instructions for results
head(rankall("heart attack", 20), 10)
head(rankall("heart attack",5000),10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
head(rankall("heart attack", 3), 5)
head(rankall("heart attack", "best"), 10)
head(rankall("hert attack",20),10)
head(rankall("heart attack","middle"),10)