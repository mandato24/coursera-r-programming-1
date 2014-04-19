corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  correlations <- numeric(0)
  files <- list.files(directory)
  for(filename in files) {
    classes <- c('Date', 'numeric', 'numeric', 'integer')
    monitor <- read.csv(paste0(directory, '/', filename),
                        colClasses=classes)
    if(sum(complete.cases(monitor)) > threshold){
      correlation <- cor(monitor[['sulfate']], monitor[['nitrate']],
                         use='complete.obs')
      correlations <- c(correlations, correlation)
    }
  }
  correlations
}