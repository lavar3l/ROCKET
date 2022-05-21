library(forecast)

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

meanify <- function(data) {
  reference_length <- calculate.reference.row.length(data, min)
  data <- meanify.internal(data, reference_length)

  return(data)
}

meanify.internal <- function(data, reference_length) {
  data <- lapply(data, function(file) {
    file$DATA <- t(apply(file$DATA, MARGIN = 1, FUN = function(row) {
      row <- row[!is.na(row)]
      dst <- abs(row - mean(row))
      top <- row[order(dst)][1:reference_length]
      idx <- unique(unlist(sapply(top, function(x) { which(row == x) } )))[1:reference_length]
      return(row[idx])
    }))

    return(list(DATA = file$DATA, CLASSES = file$CLASSES))
  })
}

extremify <- function(data) {
  reference_length <- calculate.reference.row.length(data, min)
  data <- extremify.internal(data, reference_length)

  return(data)
}

extremify.internal <- function(data, reference_length) {
  data <- lapply(data, function(file) {
    file$DATA <- t(apply(file$DATA, MARGIN = 1, FUN = function(row) {
      row <- row[!is.na(row)]
      dst <- abs(row - mean(row))
      top <- row[order(dst, decreasing = T)][1:reference_length]
      idx <- unique(unlist(sapply(top, function(x) { which(row == x) } )))[1:reference_length]
      return(row[idx])
    }))

    return(list(DATA = file$DATA, CLASSES = file$CLASSES))
  })
}

complement <- function(data) {
  reference_length <- calculate.reference.row.length(data, max)
  data <- trim.data(data, reference_length)
  data <- complement.internal(data, reference_length)

  return(data)
}

complement.internal <- function(data, reference_length) {
  data <- lapply(data, function(file) {
    file$DATA <- t(apply(file$DATA, MARGIN = 1, FUN = function(row) {
      row <- row[!is.na(row)]
      row <- c(array(row, dim = c(1, reference_length)))
      return(row)
    }))

    return(list(DATA = file$DATA, CLASSES = file$CLASSES))
  })
}

forecaster <- function(data) {
  reference_length <- calculate.reference.row.length(data, max)
  data <- trim.data(data, reference_length)
  data <- forecaster.internal(data, reference_length)

  return(data)
}

forecaster.internal <- function(data, reference_length) {
  # parallel computing options
  parallelCluster <- makeCluster(8, type = "SOCK", methods = FALSE)
  setDefaultCluster(parallelCluster)
  registerDoParallel(parallelCluster)

  data <- foreach(file = data, .export = c("auto.arima", "forecast")) %dopar% {
    file$DATA <- t(apply(file$DATA, MARGIN = 1, FUN = function(row) {
      row <- row[!is.na(row)]

      diff <- abs(reference_length - length(row))
      if (diff > 0) {
        a <- auto.arima(row)
        complement <- forecast(a, h = diff)
        row <- c(row, complement$mean)
      }

      return(row)
    }))

    return(list(DATA = file$DATA, CLASSES = file$CLASSES))
  }

  stopCluster(parallelCluster)

  return(data)
}