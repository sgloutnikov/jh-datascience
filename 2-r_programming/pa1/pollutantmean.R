pollutantmean <- function(directory, pollutant, id = 1:332) {
  files <- list.files(directory, pattern=".csv", full.names=TRUE)
  values <- numeric()
  
  #Read pollutant data values and append
  for (i in id) {
    data <- read.csv(files[i], header=TRUE)
    values <- c(values, data[[pollutant]])
  }
  
  #Compute mean for all available data and format print
  mean <- mean(values, na.rm=TRUE)
  formatC(mean, digits=3, format="f")
  print(mean)
}