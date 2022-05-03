calculate.reference.row.length <- function (data, fun) {
  fun(sapply(data, function(d) {
    fun(t(apply(d$DATA, MARGIN = 1, FUN = function(row) {
      fun(length(row[!is.na(row)]))
    })))
  }))
}

trim.data <- function(data, reference_length) {
  data <- lapply(data, function(file) {
    file$DATA <- t(apply(file$DATA, MARGIN = 1, FUN = function(row) {
      return(row[1:reference_length])
    }))

    return(list(DATA = file$DATA, CLASSES = file$CLASSES))
  })

  return(data)
}

padding <- function(data) {
  reference_length <- calculate.reference.row.length(data, max)
  data <- trim.data(data, reference_length)
  data <- padding.inner(data)

  return(data)
}

padding.inner <- function(data) {
  data <- lapply(data, function(file) {
    file$DATA <- t(apply(file$DATA, MARGIN = 1, FUN = function(row) {
      row[is.na(row)] <- -1
      return(row)
    }))

    return(list(DATA = file$DATA, CLASSES = file$CLASSES))
  })

  return(data)
}

cropping <- function(data) {
  reference_length <- calculate.reference.row.length(data, min)
  data <- trim.data(data, reference_length)

  return(data)
}