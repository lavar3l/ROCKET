library(stringr)

source('equalizers.R')
source('parse_files.R')
source('utils.R')
source('rocket_optimised.R')
source('classify.R')

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
  results <- lapply(seq_along(files), function(idx) {
    file <- files[[idx]]
    filename <- names(files)[idx]

    # send data down the pipeline
    train_data <- process.pipeline(file$TRAIN, equalizer, "TRAIN", filename)
    test_data <- process.pipeline(file$TEST, equalizer, "TEST", filename)
    train_classes <- train_data$CLASSES
    test_classes <- test_data$CLASSES

    results <- python_check.accuracy(train_data$DATA, train_classes, test_data$DATA, test_classes)

    list(
      TRAIN_DATA = train_data$DATA,
      TRAIN_CLASSES = train_classes,
      TEST_DATA = test_data$DATA,
      TEST_CLASSES = test_classes,
      ACCURACY = results$ACCURACY,
      PREDICTIONS = results$PREDICTIONS
    ) -> rocket_results

    print(paste0(c(filename, " achieved ", rocket_results$ACCURACY, " accuracy"), collapse = ""))

    save(rocket_results, file = paste0("./data_dump/", filename, ".rocket.rdata"))
    return(rocket_results)
  })

  names(results) <- names(files)
  return(results)
}

process.pipeline <- function(file_set, equalizer, split, filename) {
  data <- pre.process.file.set(file_set, equalizer)

  print(paste0(c(filename, " ", split, " was preprocessed"), collapse = ""))

  names(data) <- sapply(seq_along(data), function(x) { paste0("set", x, collapse = "") })
  td <- rocket(data, 100)

  print(paste0(c("rocket finished on: ", filename, " ", split), collapse = ""))

  return(list(DATA = td, CLASSES = data[[1]]$CLASSES))
}

# --- main ---
set.seed(seed = as.numeric(Sys.Date()), kind = "Mersenne-Twister")
files <- load.files('../data')
res <- process.files(files[1], cropping)

