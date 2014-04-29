rankhospital <- function(state, outcome, num = "best") {
  # Setup
  outcomes = c("heart attack", "heart failure", "pneumonia")
  
  # Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Check that state and outcome are valid
  if (!is.element(state, state.abb)) {
    stop("invalid state")
  }
  if (!is.element(outcome, outcomes)) {
    stop("invalid outcome")
  }
  
  # Begin extracting data
  # Which column to use
  index <- c(11, 17, 23)[outcomes == outcome]
  # Keep only data rows for 'state'
  data <- data[data$State == state,]
  # Convert to numeric for comaprison and finding NA
  suppressWarnings(data[[index]] <- as.numeric(data[[index]]))
  # Remove missing data
  data <- data[!is.na(data[[index]]),]
  # Sort by minimum index, and hospital name -- best one on top
  data <- data[order(data[index], data$Hospital.Name),]
  
  # Check what to return
  if (num == "best") {
    ret <- data$Hospital.Name[1]
  }
  else if (num == "worst") {
    ret <- data$Hospital.Name[nrow(data)]
  }
  else if (num <= nrow(data)) {
    ret <- data$Hospital.Name[num]
  }
  else {
    ret <- NA
  }
  
  # Return the the first reslt hospital name
  ret
}