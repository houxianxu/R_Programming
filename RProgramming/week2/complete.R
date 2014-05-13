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


complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  idLen <- length(id)
  nobs <- rep(0, idLen)
  j <- 1
  for (i in id) {
    tmpIdData <- getDataFromSingleId(directory, i)
    tmpCount <- sum(complete.cases(tmpIdData))
    nobs[j] <- tmpCount
    j <- j + 1
  }
  return (data.frame(id = id, nobs = nobs))
}