# load files
load_files <- function(path) {
  files <- list.files(path = path, recursive = T, all.files = T, pattern = "^[.]*TRAIN|TEST.txt$", full.names = T)
  files <- c(files, list.files(path = path, recursive = T, all.files = T, pattern = "^[.]*TEST|TRAIN.txt$", full.names = T))
  return(files)
} 

# parser functions
parse_file <- function(file) {
  file <- read.table(files[1], sep = "\n")
  file <- unlist(file)
  return(lapply(file, function(v) {parse_line(v)}))
}

parse_line <- function(line) {
  str <- unlist(strsplit(line, split = "  "))
  str <- str[str != ""]
  return(as.numeric(str))
}

generate_length <- function(i, length, fraction = 1/3) {
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
  
  return(floor(length * mean(runif(10, lower_bound, upper_bound))))
}

trim_lengths <- function(list) {
  length <- length(list)
  
  for(i in 1:length) {
    list[[i]] <- list[[i]][1:generate_length(i, length)]
  }
  
  return(list)
}

main <- parse(path) {
  files <- load_files(path)
  parsed <- parse_file(files[1])  
  parsed <- trim_lengths
}
