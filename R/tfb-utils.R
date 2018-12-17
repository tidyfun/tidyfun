# input homogenizers
df_2_df <- function(data, id = 1, arg = 2, value = 3) {
  data <- na.omit(data[, c(id, arg, value)])
  colnames(data) <- c("id", "arg", "data")
  stopifnot(
    nrow(data) > 0,
    is.numeric(data[[2]]),
    is.numeric(data[[3]])
  )
  data
}

mat_2_df <- function(x, arg) {
  stopifnot(is.numeric(x))
  id <- unique_id(rownames(x)) %||% seq_len(dim(x)[1])
  id <- ordered(id, levels = unique(id))
  df_2_df(data_frame(
    # use t(x) here so that order of vector remains unchanged...
    id = id[col(t(x))], arg = arg[row(t(x))],
    data = as.vector(t(x))
  ))
}


#' @import mgcv
smooth_spec_wrapper <- function(spec, deriv = 0, eps = 1e-6) {
  stopifnot(deriv %in% c(-1, 0, 1, 2), isTRUE(eps > 0))
  if (deriv == 0) {
    return(function(arg) {
      mgcv::Predict.matrix(object = spec, data = data.frame(arg = arg))
    })
  }
  if (deriv == 1) {
    return(function(arg) {
      X <- mgcv::Predict.matrix(
        object = spec,
        data = data.frame(arg = c(arg + eps, arg - eps))
      )
      (X[seq_along(arg), ] - X[-seq_along(arg), ]) / (2 * eps)
    })
  }
  if (deriv == 2) {
    return(function(arg) {
      g <- length(arg)
      X <- mgcv::Predict.matrix(
        object = spec,
        data = data.frame(arg = c(arg + eps, arg, arg - eps))
      )
      (X[1:g, ] - (2 * X[(g + 1):(2 * g), ]) + X[- (1:(2 * g)), ]) / eps^2
    })
  }
  if (deriv == -1) {
    return(function(arg) {
      # make sure quadrature runs over entire range up to the new arg
      # --> have to re-use original grid
      arg_orig <- spec$Xu[spec$Xu <= max(arg)]
      arg_interleave <- sort(unique(c(arg_orig, arg)))
      new_args <- which(arg_interleave %in% arg)
      X <- mgcv::Predict.matrix(
        object = spec,
        data = data.frame(arg = arg_interleave)
      )
      apply(X, 2, function(arg, x) cumsum(quad_trapez(arg, x)),
            arg = arg_interleave
      )[new_args, ]
    })
  }
}
