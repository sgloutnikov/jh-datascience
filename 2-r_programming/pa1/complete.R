complete <- function(directory, id = 1:332) {
  files <- list.files(directory, pattern=".csv", full.names=TRUE)
  idRow <- numeric()
  nobsRow <- numeric()
  
  #Read data, sum complete cases and append to row and nobs columns
  for (i in id) {
    data <- read.csv(files[i], header=TRUE)
    idRow <- c(idRow, i)
    nobsRow <- c(nobsRow, sum(complete.cases(data)))
  }
  
  #Transform to data frame and print
  df <- data.frame(id = idRow, nobs = nobsRow)
  print(df)
}