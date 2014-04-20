corr <- function(directory, threshold = 0) {
  files <- list.files(directory, pattern=".csv", full.names=TRUE)
  corResult <- numeric()
  
  for (i in 1:length(files)) {
    data <- read.csv(files[i], header=TRUE, colClasses=c("NULL", NA, NA, "NULL"))
    #Append to the corrolation vector if threshold of complete data is met
    if (sum(complete.cases(data)) > threshold) {
      sulfateComplete <- data[complete.cases(data),1]
      nitrateComplete <- data[complete.cases(data), 2]
      corResult <- c(corResult, cor(sulfateComplete, nitrateComplete))
    }
  }
  
  corResult
}
