library(stringr)

setwd('./rocket')

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

match.alpha <- function(file, filename, equalizer) {
  train_data <- process.pipeline(file$TRAIN, equalizer, "TRAIN", filename, 50)
  test_data <- process.pipeline(file$TEST, equalizer, "TEST", filename, 50)
  train_classes <- train_data$CLASSES
  test_classes <- test_data$CLASSES

  alphas <- 1:9
  accuracies <- sapply(alphas, FUN = function(alpha) {
    res <- python_check.accuracy(train_data$DATA, train_classes, test_data$DATA, test_classes, alpha)
    return(res$ACCURACY)
  })
  accuracies <- unlist(accuracies)
  idx <- which(accuracies == max(accuracies))[1]

  return(list(MATCHED = alphas[idx], ALPHAS = alphas, ACCURACIES = accuracies))
}

extensive.processing <- function(file, equalizer, filename, equalizer_name, extensive_data_dump = F, length = 10, kernels = 100) {
  # one pre-run to find the best alpha
  # run test for 50 kernels and try alphas from 1 to 9,
  # choose the best and apply it for the rest of measurements
  matched_alpha <- match.alpha(file, filename, equalizer)

  print(paste0(c("alpha matched for file ", filename, " to be: ", matched_alpha$MATCHED), collapse = ""))

  data_dump <- lapply(1:length, function(idx) {
    train_data <- process.pipeline(file$TRAIN, equalizer, "TRAIN", filename, kernels)
    test_data <- process.pipeline(file$TEST, equalizer, "TEST", filename, kernels)
    train_classes <- train_data$CLASSES
    test_classes <- test_data$CLASSES

    results <- python_check.accuracy(train_data$DATA, train_classes, test_data$DATA, test_classes, matched_alpha$MATCHED)

    print(paste0(c("processing #", idx, " finished for ", filename), collapse = ""))

    list(
      TRAIN_DATA = train_data$DATA,
      TRAIN_CLASSES = train_classes,
      TEST_DATA = test_data$DATA,
      TEST_CLASSES = test_classes,
      ACCURACY = results$ACCURACY,
      PREDICTIONS = results$PREDICTIONS,
      TEST_R_TIME = test_data$R_TIME,
      TEST_P_TIME = test_data$P_TIME,
      TRAIN_R_TIME = train_data$R_TIME,
      TRAIN_P_TIME = train_data$P_TIME
    )
  })

  results <- sapply(data_dump, function(d) { d$ACCURACY })
  test_r_time <- sapply(data_dump, FUN = function(d) { d$TEST_R_TIME[3] })
  test_p_time <- sapply(data_dump, FUN = function(d) { d$TEST_P_TIME[3] })
  train_r_time <- sapply(data_dump, FUN = function(d) { d$TRAIN_R_TIME[3] })
  train_p_time <- sapply(data_dump, FUN = function(d) { d$TRAIN_P_TIME[3] })

  names(data_dump) <- sapply(1:length, FUN = function(idx) { paste0(c("test", idx), collapse = "") })
  data_dump <- append(data_dump, list(results = results, matched_alpha = matched_alpha))

  if (extensive_data_dump == T) { save(data_dump, file = paste0(c("./data_dump/extensive.processing/", filename, "_", equalizer_name, "_dump.rdata"), collapse = "")) }

  return(c(filename, mean(results), max(results), min(results), median(results), matched_alpha$MATCHED, kernels, mean(test_r_time), mean(test_p_time), mean(train_r_time), mean(train_p_time)))
}

process.files <- function(files, extensive_data_dump = F, length = 10, kernels = 100) {
  equalizers <- list(
    list(EQ = padding, NAME = "padding"),
    list(EQ = cropping, NAME = "cropping"),
    list(EQ = meanify, NAME = "meanify"),
    list(EQ = extremify, NAME = "extremify"),
    list(EQ = complement, NAME = "complement")
    #list(EQ = forecaster, NAME = "forecaster")
  )

  lapply(equalizers, function(equalizer) {
    lapply(seq_along(files), function(idx) {
      file <- files[[idx]]
      filename <- names(files)[idx]

      extensive.processing(file, equalizer$EQ, filename, equalizer$NAME, extensive_data_dump, length, kernels)
    }) -> temp

    file_data <- data.frame()
    for (i in seq_along(temp)) {
      file_data <- rbind(file_data, temp[[i]])
    }

    colnames(file_data) <- c(
      "filename",
      "accuracy mean",
      "accuracy max",
      "accuracy min",
      "accuracy median",
      "alpha",
      "kernels",
      "test rocket time",
      "test preprocessing time",
      "train rocket time",
      "train preprocessing time"
    )

    write.csv(file_data, file = paste0(c("./results/", equalizer$NAME, ".csv"), collapse = ""))
    return(file_data)
  })
}

process.pipeline <- function(file_set, equalizer, split, filename, kernels = 100) {
  pre_processing_time <- system.time({ data <- pre.process.file.set(file_set, equalizer) })

  print(paste0(c(filename, " ", split, " was preprocessed"), collapse = ""))

  names(data) <- sapply(seq_along(data), function(x) { paste0("set", x, collapse = "") })
  rocket_time <- system.time({ td <- rocket(data, kernels) })

  print(paste0(c("rocket finished on: ", filename, " ", split), collapse = ""))

  return(list(DATA = td, CLASSES = data[[1]]$CLASSES, P_TIME = pre_processing_time, R_TIME = rocket_time))
}

# --- main ---
set.seed(seed = as.numeric(Sys.Date()), kind = "Mersenne-Twister")

files <- load.files('../data')
res <- process.files(files[1:2], T, 10, 100)
print(res)
