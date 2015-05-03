rankall <- function(outcome, num = "best") {

  #load heathcare.gov outcomes data
  outcomeCSV <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  suppressWarnings(outcomeCSV[, 11] <- as.numeric(outcomeCSV[, 11]))        #coerce heart attack to numeric
  suppressWarnings(outcomeCSV[, 17] <- as.numeric(outcomeCSV[, 17]))        #coerce heart failure to numeric  
  suppressWarnings(outcomeCSV[, 23] <- as.numeric(outcomeCSV[, 23]))        #coerce pneumonia to numeric
  
  #First check outcome for validity
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop("invalid outcome")
  } 
  
  #Create vector of all States
  stateLevels <- levels(factor(outcomeCSV$State))
  
  
  #set metric column number
  if (outcome == "heart attack"){
    outcomeCol <- 11
  } else if (outcome == "heart failure"){
    outcomeCol <- 17
  } else if (outcome == "pneumonia"){
    outcomeCol <- 23
  } 
  
  #create dataframe for results
  results <- data.frame(hospital= character(), state= character())
  
  #store num attribute entered - this is required given num evaluation code in order to accomodate differing "worst"s
  cacheNum <- num
  
  for (ST in stateLevels) {
    
    #subset for State attribute  
    subState <- outcomeCSV[which(outcomeCSV$State == ST),]
    
    #sort state subset by outcome then by Hospital.Name alpha a-z
    subState <- subState[order(subState[,outcomeCol], subState$Hospital.Name),]
    
    #evaluate num and coerce as needed
    num <- cacheNum
    if (num == "best") {
      num <- as.integer(1)
    } else if (num == "worst") {
      num <- as.integer(which.max(subState[,outcomeCol]))    
    } else {
      num <- as.integer(num)
    }  
    
    #return first hospital in with the requested rank for the given outcome and state
    results <- rbind(results, data.frame(hospital=subState[num, "Hospital.Name"], state=ST))
    
  }
  
  rownames(results) <- stateLevels
  results 
}