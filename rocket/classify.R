library(reticulate)
use_python("/usr/bin/python3")
#use_python("../python/venv/Scripts/python.exe")

python_pickle.dump <- function(object, filename) {
  pickler <- import("pickle")
  file <- builtins$open(filename, "wb")
  pickler$dump(object, file)
  file$close()
}

remove.infinity <- function(data) {
  for (i in seq_along(data)) {
    if (is.infinite(data[i]) || is.nan(data[i])) {
      data[i] <- -1
    }
  }

  return(data)
}

python_check.accuracy <- function(train_data, train_classes, test_data, test_classes, alpha = array(c(0.1, 1.0, 10.0))) {
  train_data <- remove.infinity(train_data)
  test_data <- remove.infinity(test_data)
  
  sklearn_model <- import("sklearn.linear_model")
  sklearn_metrics <- import("sklearn.metrics")
  sklearn_pipeline <- import("sklearn.pipeline")
  sklearn_preprocessing <- import("sklearn.preprocessing")

  classifier <- sklearn_pipeline$make_pipeline(
    sklearn_preprocessing$StandardScaler(with_mean=F),
    sklearn_model$RidgeClassifierCV(alpha)
  )
  classifier$fit(train_data, train_classes)

  predictions <- classifier$predict(test_data)
  accuracy <- sklearn_metrics$accuracy_score(predictions, test_classes)

  return(list(ACCURACY = accuracy, PREDICTIONS = predictions))
}