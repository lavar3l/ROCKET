library("foreign")

pre.process.file.set <- function(filenames, equalizer) {
  # returns:
  # set: list of 2:
  #   $DATA: data.frame with parsed data
  #   $CLASSES: vector (numeric) with results

  # read files' contents
  set <- lapply(filenames, function(filename) { read.file(filename) })
  # equalize variable lengths
  set <- equalizer(set)
}

read.file <- function(filename) {
  # separate results (classes) from data
  raw_data <- read.arff(filename)
  classes <- raw_data[,length(raw_data)]
  raw_data <- raw_data[,1:(length(raw_data) - 1)]
  raw_data <- trim.lengths(raw_data)

  list(DATA = raw_data, CLASSES = classes)
}

trim.lengths <- function(data) {
  length <- dim(data)[1]
  row_l <- dim(data)[2]
  lengths <- sapply(1:length, function(i) { generate.length(i, length, row_l) })

  for (i in 1:length) {
    data[i,(lengths[i] + 1):row_l] <- NA
  }

  return(data)
}

generate.length <- function(i, length, data_length, fraction = 1/3) {
  if (i < fraction * length && i > 0) {
    part <- 1
  } else if (i < 2 * fraction * length && i > fraction * length) {
    part <- 2
  } else {
    part <- 3
  }
  
  lower_bounds <- c(0.1, 0.4, 0.7)
  upper_bounds <- c(0.4, 0.7, 1)
  
  lower_bound <- lower_bounds[part]
  upper_bound <- upper_bounds[part]
  
  return(floor(data_length * mean(runif(10, lower_bound, upper_bound))))
}
