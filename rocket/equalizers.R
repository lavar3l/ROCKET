padding <- function(data) {
  reference_length <- max(sapply(data, function(d) { max(sapply(d$DATA, function(col) { max(length(col)) })) }))
  lapply(data, function(d) { list(DATA = padding.inner(d$DATA, reference_length), CLASSES = d$CLASSES) })
}

cropping <- function(data) {
  reference_length <- min(sapply(data, function(d) { min(sapply(d$DATA, function(col) { max(length(col)) })) }))
  lapply(data, function(d) { list(DATA = cropping.inner(d$DATA, reference_length), CLASSES = d$CLASSES) })
}

padding.inner <- function(raw_data, reference_length) {
  mx <- reference_length

  for (i in seq_along(raw_data)) {
    len <- length(raw_data[[i]])
    if (len != mx)
      raw_data[[i]] <- c(raw_data[[i]], rep(-1, mx - len))
  }

  return(as.data.frame(raw_data))
}

cropping.inner <- function(raw_data, reference_length) {
  mn <- reference_length

  for (i in seq_along(raw_data)) {
    raw_data[[i]] <- raw_data[[i]][1:mn]
  }
}