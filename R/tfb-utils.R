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

#-------------------------------------------------------------------------------

fit_unpenalized <- function(data, spec_object, arg_u, regular) {
  eval_list <- split(data$data, data$id)
  if (regular) {
    eval_matrix <- do.call(cbind, eval_list)
    qr_basis <- qr(spec_object$X)
    coef_list <- qr.coef(qr = qr_basis, y = eval_matrix)
    coef_list <- split(coef_list, col(coef_list))
    pve <- 1 - apply(qr.resid(
      qr = qr_basis,
      y = eval_matrix
    ), 2, var) / apply(eval_matrix, 2, var)
  } else {
    index_list <- split(attr(arg_u, "index"), data$id)
    coef_list <- map2(
      index_list, eval_list,
      ~ qr.coef(qr = qr(spec_object$X[.x, ]), y = .y)
    )
    pve <- unlist(map2(
      index_list, eval_list,
      ~1 - var(qr.resid(qr = qr(spec_object$X[.x, ]), y = .y)) / var(.y)
    ))
  }
  names(coef_list) <- levels(data$id)
  return(list(coef = coef_list, pve = pve))
}


fit_penalized <- function(data, spec_object, arg_u, gam_args, regular, global) {
  eval_list <- split(data$data, data$id)
  index_list <- split(attr(arg_u, "index"), data$id)
  coef_list <- map2(
    index_list, eval_list,
    ~ magic_smooth_coef(.y, .x, spec_object, magic_args)
  )
  pve <- unlist(map(coef_list, 2))
  coef_list <- map(coef_list, 1)
  names(coef_list) <- levels(data$id)
  return(list(coef = coef_list, pve = pve))
}


magic_smooth_coef <- function(evaluations, index, spec_object, magic_args) {
  m <- do.call(
    mgcv::magic,
    c(
      list(y = evaluations, X = spec_object$X[index, ], S = spec_object$S),
      flatten(list(off = 1, magic_args))
    )
  )
  list(coef = m$b, pve = 1 - m$scale / var(evaluations))
}
