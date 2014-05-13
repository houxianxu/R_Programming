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

pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  lenId <- length(id)
  pollutantSumVec <- rep(0, lenId)
  countVec <- rep(0, lenId)
  j <- 1
 
  for (i in id) {
    idData <- getDataFromSingleId(directory, i)
    idSum <- sum(idData[pollutant], na.rm = TRUE) ## can't us idData$pollutant
    count <- sum(!is.na(idData[pollutant]))
    pollutantSumVec[j] <- idSum
    countVec[j] <- count
    j <- j + 1
  }
  return (sum(pollutantSumVec) / sum(countVec))
}
