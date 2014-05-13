getDataFromSingleId <- function(directory, id) {
  ## get the new id with 3 digit
  if (id < 10) {
    newId <- paste("00", id, sep = "")
  } else if (id >= 10 & id < 100) {
    newId <- paste("0", id, sep ="")
  } else if (id >= 100 & id < 1000) {
    newId <- id
  }
  
  newDir <- paste(directory, "/", newId, ".csv", sep = "")
  idData <- read.csv(newDir, header = TRUE)
  return (idData)
}

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  completeData <- complete(directory)
  overThreshold <- completeData[completeData$nobs > threshold, ]$id
  lenRes <- length(overThreshold)
  result <- rep(0,lenRes)
  
  j <- 1
  for (i in overThreshold) {
    tempData <- getDataFromSingleId(directory, i)
    correlation <- cor(tempData$sulfate, tempData$nitrate, use="complete.obs")
    result[j] <- correlation
    j <- j + 1
  }
  return (result)
}
