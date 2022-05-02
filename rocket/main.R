library(stringr)

source('equalizers.R')
source('parse_files.R')
source('utils.R')
source('rocket_optimised.R')

# load files
load.files <- function(path) {
  # returns file structure in a list that is organized as follows:
  # files: List of N lists
  #   $dirname1: List of 2 vectors
  #     $TRAIN: vector of chr (with train filenames)
  #     $TEST: vector of chr (with test filenames)
  #   $dirname2: [...]
  #   ...
  #   $dirnameN: [...]
  #

  dirs <- list.dirs(path = path, recursive = T)
  dirs <- dirs[2:length(dirs)] # first dir is the main dir - omit

  files <- lapply(dirs, function(dir) {
    train_files <- list.files(path = dir, recursive = T, all.files = T, pattern = "^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]*[0123456789]{1}_TRAIN.arff$", full.names = T)
    test_files <- list.files(path = dir, recursive = T, all.files = T, pattern = "^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]*[0123456789]{1}_TEST.arff$", full.names = T)

    list(TRAIN = train_files, TEST = test_files)
  })

  dirnames <- sapply(dirs, function(dir) {
    dirname <- unlist(str_split(dir, '/'))
    dirname <- dirname[length(dirname)]
  })

  names(files) <- dirnames
  return(files)
}

process.files <- function(files, equalizer) {
  results <- lapply(files, function(file) {
    # send data down the pipeline
    train_data <- process.pipeline(file$TRAIN, equalizer)
    test_data <- process.pipeline(file$TEST, equalizer)

    list(TRAIN = train_data, TEST = test_data)
  })

  names(results) <- names(files)
  return(results)
}

process.pipeline <- function(file_set, equalizer) {
  data <- pre.process.file.set(file_set, equalizer)
  names(data) <- sapply(seq_along(data), function(x) { paste0("set", x, collapse = "") })
  rocket_iter(data)

  return(data)
}

# --- main ---
files <- load.files('../data')
res <- process.files(files[1], padding)