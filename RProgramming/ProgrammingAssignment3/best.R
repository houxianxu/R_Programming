best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!state %in% data$State) {
    stop("invalid state")
  }
  
  outcomeCol <- 0
  if (outcome == outcomes[1]) {
    outcomeCol <- 11
  } else if (outcome == outcomes[2]) {
    outcomeCol <- 17
  } else if (outcome == outcomes[3]) {
    outcomeCol <- 23
  } else {
    stop("invalid outcome")
  }
  
  ## subset the useful data
  ## and using "order" to get the result
  newFrame <- data.frame(hosName = data[, 2], newState = data$State, newOutcome = as.numeric(data[, outcomeCol]), stringsAsFactors = FALSE)
  newFrame <- newFrame[newFrame$newState == state, ]
  newFrame <- newFrame[order(newFrame$newOutcome, newFrame$hosName), ]
  return (newFrame$hosName[1])
}
