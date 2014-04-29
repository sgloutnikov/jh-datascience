rankall <- function(outcome, num = "best") {
  # Setup
  outcomes = c("heart attack", "heart failure", "pneumonia")
  
  # Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Check that outcome id valid
  if (!is.element(outcome, outcomes)) {
    stop("invalid outcome")
  }
  
  # Begin extracting data
  # Which column to use
  index <- c(11, 17, 23)[outcomes == outcome]
  # Keep only data rows for 'state'
  # data <- data[data$State == state,]
  # Convert to numeric for comaprison and finding NA
  suppressWarnings(data[[index]] <- as.numeric(data[[index]]))
  # Remove missing data
  data <- data[!is.na(data[[index]]),]
  
  
  
  hospitals <- vector()
  states <- vector()
  i <- 0
  for (state in state.abb) {
    # Keep only data rows for 'state'
    stateData <- data[data$State == state,]
    # Sort by minimum index, and hospital name -- best one on top
    stateData <- stateData[order(stateData[index], stateData$Hospital.Name),]
    
    # Check what to return
    if (num == "best") {
      ret <- stateData$Hospital.Name[1]
    }
    else if (num == "worst") {
      ret <- stateData$Hospital.Name[nrow(stateData)]
    }
    else if (num <= nrow(stateData)) {
      ret <- stateData$Hospital.Name[num]
    }
    else {
      ret <- NA
    }
    hospitals[i] <- ret
    states[i] <- state
    i <- i+1
  }
  
  tmp <- data.frame(hospital=hospitals, state=states)
  tmp
}

