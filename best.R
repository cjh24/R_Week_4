best <- function(state, outcome) {
  
  outcomeCSV <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  suppressWarnings(outcomeCSV[, 11] <- as.numeric(outcomeCSV[, 11]))
  
  #First check outcome for validity
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop("invalid outcome")
  } 
  
  #Then check state for validity 
  
  oStates <- levels(factor(outcomeCSV$State))
  
  if (!state %in% oStates ){
    stop("invalid state")
  } 
 
#Enter function work here
  
}
    

  
  
