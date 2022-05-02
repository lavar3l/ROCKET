library(geometry)

# parallel computing
library(foreach)
library(doParallel)

generate.lengths <- function(how_much) {
  # sapply(1:how_much, function(index) { sample(c(11), 1) })
  sapply(1:how_much, function(index) { sample(c(7, 9, 11), 1) })
}

generate.channel.indices <- function(how_much, num_columns, lengths) {
  sapply(1:how_much, function(index)
  {
    limit <- min(lengths[index], num_columns)
    #return(5)
    return(floor(2^(runif(1,0, log2(limit + 1)))))
  }
  )
}

generate.kernels <- function(timepoints, num_kernels, num_columns, seed = NULL) {
  if (!is.null(seed))
    set.seed(seed)

  lengths <- generate.lengths(num_kernels)
  num_channel_indices <- generate.channel.indices(num_kernels, num_columns, lengths)
  channel_indices <- rep(0, sum(num_channel_indices))
  weights <- rep(0, dot(x = lengths, y = num_channel_indices))
  biases <- rep(0, num_kernels)
  dilations <- rep(0, num_kernels)
  paddings <- rep(0, num_kernels)

  a1 <- 0
  a2 <- 0

  for (i in 0:(num_kernels - 1)) {
    length_ <- lengths[i + 1]
    num_channel_indices_ <- num_channel_indices[i + 1]
    weights_ <- rnorm(num_channel_indices_ * length_,0, 1)
    #weights_ <- rep(0.5, num_channel_indices_ * length_)
    b1 <- a1 + (num_channel_indices_ * length_)
    b2 <- a2 + num_channel_indices_

    a3 <- 0

    for (idx in (0:num_channel_indices_ - 1)) {
      b3 <- a3 + length_
      weights_[(a3 + 1):b3] <- weights_[(a3 + 1):b3] - mean(weights_[(a3 + 1):b3])
      a3 <- b3
    }

    # replace weights[(a1 + 1):b1] with weights_
    # weights[(a1 + 1):b1] <- weights_ (in python)
    temp_w <- c()
    if (a1 > 0) {
      temp_w <- weights[1:a1]
    }

    temp_w <- c(temp_w, weights_)

    if (b1 < length(weights)) {
      temp_w <- c(temp_w, weights[(b1 + 1):length(weights)])
    }

    weights <- temp_w

    channel_indices[(a2 + 1):b2] <- sample(0:(num_columns - 1), num_channel_indices_, replace = F)
    biases[i + 1] <- runif(1, -1, 1)

    # channel_indices[(a2 + 1):b2] <- rep(0, num_channel_indices_)
    # biases[i + 1] <- 0

    dilation <- floor(2^runif(1, 0, log2((timepoints - 1) / length_ - 1)))
    # dilation <- 4
    dilations[i + 1] <- dilation

    padding <- if (sample(c(0,1),1) == 1) floor(((length_ - 1) * dilation) / 2) else 0
    # padding <- 0
    paddings[i + 1] <- padding

    a1 <- b1
    a2 <- b2
  }

  return(list(
    weights = weights,
    lengths = lengths,
    biases = biases,
    dilations = dilations,
    paddings = paddings,
    num_channel_indices = num_channel_indices,
    channel_indices = channel_indices
  ))
}

apply.kernel.multivariate <- function(data, weights, length, bias, dilation, padding, num_channel_indices, channel_indices) {
  num_columns <- dim(data)[1]
  num_timepoints <- dim(data)[2]
  output_length <- (num_timepoints + (2 * padding)) - ((length - 1) * dilation)
  ppv_ <- 0
  max_ <- -Inf
  end <- (num_timepoints + padding) - ((length - 1) * dilation)

  for (i in -padding:(end - 1)) {
    sum_ <- bias
    index <- i

    for (j in (0:(length - 1))) {
      if (index > 0 && index < num_timepoints) {
        for (k in 0:(num_channel_indices - 1)) {
          sum_ <- sum_ + weights[(k + 1), (j + 1)] * data[(channel_indices[k + 1] + 1), (index + 1)]
        }
      }

      index <- index + dilation
    }

    if (sum_ > max_) {
      max_ <- sum_
    }

    if (sum_ > 0) {
      ppv_ <- ppv_ + 1
    }
  }

  return(c(ppv_ / output_length, max_))
}

prepare_a1 <- function(kernels) {
  a1 <- cumsum(c(0, kernels$num_channel_indices * kernels$lengths))
  a1 <- lapply(1:(length(a1) - 1), function(idx) { return(c(a1[idx], a1[idx + 1])) })
  return(a1)
}

prepare_a2 <- function(kernels) {
  a2 <- cumsum(c(0, kernels$num_channel_indices))
  a2 <- lapply(1:(length(a2) - 1), function(idx) { return(c(a2[idx], a2[idx + 1])) })
  return(a2)
}

apply.kernels <- function(data, kernels) {
  num_instances <- dim(data)[1]
  num_columns <- dim(data)[2]
  num_kernels <- length(kernels$lengths)
  #X_ <- matrix(x, num_instances, num_kernels * 2)
  X_ <- array(0, dim = c(num_instances, num_kernels * 2))

  # parallel computing options
  parallelCluster <- makeCluster(4, type = "SOCK", methods = FALSE)
  setDefaultCluster(parallelCluster)
  registerDoParallel(parallelCluster)

  rows <- split(X_, row(X_))
  i <- 0:(num_instances - 1)
  j <- 1:num_kernels
  a1 <- cumsum(c(0, kernels$num_channel_indices * kernels$lengths))
  a2 <- cumsum(c(0, kernels$num_channel_indices))

  a1 <- lapply(1:(length(a1) - 1), function(idx) { return(c(a1[idx], a1[idx + 1])) })
  a2 <- lapply(1:(length(a2) - 1), function(idx) { return(c(a2[idx], a2[idx + 1])) })

  X_ <- foreach(row_ = rows, i = i, .combine = rbind) %:%
    foreach(field = row_,
            j = 1:num_kernels,
            a1 = prepare_a1(kernels),
            a2 = prepare_a2(kernels),
            .export = c("kernels", "data", "apply.kernel.multivariate"),
            .combine = c) %dopar% {
      weights_ <- kernels$weights[(a1[1] + 1):a1[2]]
      # R reshapes arrays differently to Python,
      # switch colnums with rownums and then transpose
      weights_ <- t(array(weights_, dim = c(kernels$lengths[j], kernels$num_channel_indices[j])))

      res <- apply.kernel.multivariate(
        data = data[(i + 1),,],
        weights = weights_,
        length = kernels$lengths[j],
        bias = kernels$biases[j],
        dilation = kernels$dilations[j],
        padding = kernels$paddings[j],
        num_channel_indices = kernels$num_channel_indices[j],
        channel_indices = kernels$channel_indices[(a2[1] + 1):a2[2]]
      )

      return(res)
    }

  stopCluster(parallelCluster)

  return(X_)
}

transform.data <- function(data) {
  ret <- array(dim = c(dim(data[[1]]$DATA)[1], length(data), dim(data[[1]]$DATA)[2]))

  # fill out the array
  for (i in seq_along(data)) {
    for (j in seq_along(data[[i]]$DATA)) {
      ret[, i, j] <- data[[i]]$DATA[[j]]
    }
  }

  return(ret)
}

rocket_iter <- function(raw_data, num_kernels = 50) {
  data <- transform.data(raw_data)
  num_columns <- dim(data)[2]
  num_timepoints <- dim(data)[3]

  kernels <- generate.kernels(num_timepoints, num_kernels, num_columns)
  # save(kernels, file = get.data.dump.filepath("temp.kernels.rdata"))

  # load(get.data.dump.filepath("temp.kernels.rdata"))

  transformed_data <- apply.kernels(data, kernels)
  # save(transformed_data, file = get.data.dump.filepath("temp.rocket.results_test.rdata"))
  # python_pickle.dump(transformed_data, get.data.dump.filepath("python_dump.rocket_test.results"))
}