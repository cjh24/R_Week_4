best <- function(state, outcome) {
  
  #load heathcare.gov outcomes data
  outcomeCSV <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  suppressWarnings(outcomeCSV[, 11] <- as.numeric(outcomeCSV[, 11]))        #coerce heart attack to numeric
  suppressWarnings(outcomeCSV[, 17] <- as.numeric(outcomeCSV[, 17]))        #coerce heart failure to numeric  
  suppressWarnings(outcomeCSV[, 23] <- as.numeric(outcomeCSV[, 23]))        #coerce pneumonia to numeric
  
  #First check outcome for validity
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop("invalid outcome")
  } 
  
  #Then check state for validity 
  if (!state %in% levels(factor(outcomeCSV$State))){
    stop("invalid state")
  } 
 
  
#Now begin the actual work

#set metric column number
  if (outcome == "heart attack"){
    outcomeCol <- 11
  } else if (outcome == "heart failure"){
    outcomeCol <- 17
  } else if (outcome == "pneumonia"){
    outcomeCol <- 23
  } 
  


  #subset for State attribute  
  subState <- outcomeCSV[which(outcomeCSV$State == state),]
  
  #sort state subset by Hospital.Name alpha a-z
  subState <- subState[order(subState$Hospital.Name),]
  
  #return first hospital in with the min for the given outcome and state
  subState [which.min(subState[,outcomeCol]),
            "Hospital.Name"] 
  
}
  
