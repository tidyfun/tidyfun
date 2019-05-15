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
  df_2_df(data.frame(
    # use t(x) here so that order of vector remains unchanged...
    id = id[col(t(x))], arg = arg[row(t(x))],
    data = as.vector(t(x)),
    stringsAsFactors = FALSE
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
      (X[1:g, ] - (2 * X[(g + 1):(2 * g), ]) + X[-(1:(2 * g)), ]) / eps^2
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
# utility functions for unpenalized spline representation: least squares & GLM
fit_unpenalized <- function(data, spec_object, gam_args, arg_u, regular, 
                            ls_fit) {
  if (ls_fit) {
    return(fit_unpenalized_ls(data, spec_object, arg_u, regular))
  }  
  fit_unpenalized_glm(data, spec_object, gam_args, arg_u)
}

fit_unpenalized_glm <- function(data, spec_object, gam_args, arg_u) {
  eval_list <- split(data$data, data$id)
  index_list <- split(attr(arg_u, "index"), data$id)
  arg_u$X <- spec_object$X
  gam_prep <- do.call(gam, 
                      c(list(formula = x ~ 0 + X, data = arg_u), 
                        fit = FALSE, gam_args))
  ret <- map2(
    index_list, eval_list, 
    ~ purrr::possibly(fit_unpenalized_glm_once, 
                      quiet = FALSE,
                      otherwise = list(
                        coef = rep(NA_real_, ncol(spec_object$X)),
                        pve = NA_real_))(.x, .y, gam_prep = gam_prep)
  )
  names(ret) <- levels(data$id)
  pve <- unlist(map_dbl(ret, "pve"))
  failed <- which(is.na(pve))
  if (length(failed) > 0) {
    stop("Basis representation failed for entries:\n ", 
         paste(unname(failed), collapse = ", "))
  }
  list(coef = map(ret, "coef"), pve = pve)
}


fit_unpenalized_glm_once <- function(index, evaluations, gam_prep) {
  G_tmp <- gam_prep
  G_tmp$X <- G_tmp$X[index, ]
  G_tmp$y <- evaluations
  G_tmp$n <- length(evaluations)
  G_tmp$w <- rep(1, G_tmp$n)
  G_tmp$mf$x <- G_tmp$y
  G_tmp$offset <- rep(0, G_tmp$n)
  G_tmp$sp <- NULL #gam expects this to be nameable otherwise
  m <- gam(G = G_tmp)
  list(coef = unname(m$coefficients),
       pve = (m$null.deviance - m$deviance)/m$null.deviance)
}


fit_unpenalized_ls <- function(data, spec_object, arg_u, regular) {
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

#-------------------------------------------------------------------------------
# utility functions for penalized spline representation: 
# global fit, curve-specific LS, curve-specific GLM
fit_penalized <- function(data, spec_object, gam_args, arg_u, regular, global, 
                          ls_fit) { 
  if (global) {
    return(fit_penalized_global(data, spec_object, gam_args))
  }
  if (!ls_fit) {
    ## return(fit_penalized_local
  }
  fit_penalized_ls_local(data, spec_object, arg_u, gam_args, regular)
}

fit_penalized_ls_local <- function(data, spec_object, arg_u, gam_args, regular) {
  eval_list <- split(data$data, data$id)
  index_list <- split(attr(arg_u, "index"), data$id)
  ret <- map2(
    index_list, eval_list,
    ~ magic_smooth_coef(.y, .x, spec_object, magic_args)
  )
  pve <- unlist(map(ret, "pve"))
  coef_list <- map(ret, "coef")
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

#mgcv_smooth_coef <- function()
  
