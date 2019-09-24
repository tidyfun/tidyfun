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
  fit_ml(data, spec_object, gam_args, arg_u, penalized = FALSE)
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
  
  if (global & gam_args$sp == -1) {
    # find a suitable global level of smoothing based on a pilot estimate
    # uses 10% of curves, at most 100, at least 5
    # uses median of the smothing parameters on this pilot sample.
    pilot_id <- round(seq(from = 1, to = nlevels(data$id), 
                    length = max(1, 
                                 min(max(5, 0.1 * nlevels(data$id)), 100))))
    pilot_id <- levels(data$id)[unique(pilot_id)]
    arg_u_pilot <- arg_u
    attr(arg_u_pilot, "index") <- attr(arg_u_pilot, "index")[data$id %in% pilot_id]
    data_pilot <- subset(data, id %in% pilot_id)  %>% droplevels
    if (!ls_fit) {
      pilot_sp <- 
        fit_ml(
          data_pilot, spec_object, gam_args, arg_u_pilot, penalized = TRUE
          )$sp
    } else {
      pilot_sp <- 
        fit_penalized_ls(
          data_pilot, spec_object, arg_u_pilot, gam_args, regular
          )$sp
    }
    gam_args$sp <- exp(mean(log(pilot_sp))) #median?
  }
  if (!ls_fit) {
    return(fit_ml(data, spec_object, gam_args, arg_u, penalized = TRUE, 
                  sp = gam_args$sp))
  } 
  fit_penalized_ls(data, spec_object, arg_u, gam_args, regular)
}

fit_penalized_ls <- function(data, spec_object, arg_u, gam_args, regular) {
  eval_list <- split(data$data, data$id)
  index_list <- split(attr(arg_u, "index"), data$id)
  gam_args <- gam_args[names(gam_args) %in% names(formals(mgcv::magic))]
  ret <- map2(
    index_list, eval_list,
    ~ purrr::possibly(magic_smooth_coef, 
                        quiet = FALSE,
                        otherwise = list(
                          coef = rep(NA_real_, ncol(spec_object$X)),
                          pve = NA_real_,
                          sp = NA_real_))
                        (.y, .x, spec_object, gam_args)
  )
  sp <- unlist(map(ret, "sp"))
  pve <- unlist(map(ret, "pve"))
  coef_list <- map(ret, "coef")
  names(coef_list) <- levels(data$id)
  return(list(coef = coef_list, pve = pve, sp = sp))
}
magic_smooth_coef <- function(evaluations, index, spec_object, gam_args) {
  fixed_sp <- gam_args$sp != -1
  magic_args <- c(
    list(y = evaluations, 
         X = spec_object$X[index, ], 
         S = if (fixed_sp) list() else spec_object$S,
         H = if (fixed_sp) gam_args$sp * spec_object$S[[1]] else NULL),
    flatten(list(off = 1, gam_args))
  )
  m <- do.call(mgcv::magic, magic_args)
  list(coef = m$b, pve = 1 - m$scale / var(evaluations), sp = m$sp)
}

# fit gam for one curve, with estimated (default, sp=-1) or fixed penalization
#  or unpenalized 
fit_ml <- function(data, spec_object, gam_args, arg_u, penalized, sp = -1) {
  eval_list <- split(data$data, data$id)
  index_list <- split(attr(arg_u, "index"), data$id)
  arg_u$X <- spec_object$X
  if (penalized) {
    gam_args$paraPen <- quote(list(X = spec_object$S))
    gam_args$sp <- NULL #weirdness ensues otherwise, restored below
  }  
  gam_prep <- do.call(gam, 
                      c(list(formula = x ~ 0 + X, data = arg_u), 
                        fit = FALSE, gam_args))
  gam_prep$sig2 <- -1 # GCV switch --
                      # needs to be set explicitly so magic does not get NA
                      # as 11th argument when called from gam.fit
  if (!penalized) {
    fixed_sp <- TRUE
    gam_prep$sp <- NULL #gam expects this to be nameable otherwise
    sp <- NULL
  } else {
    fixed_sp <- sp != -1
    if (fixed_sp) {
      gam_prep$S <- list()
      gam_prep$H <- sp * spec_object$S[[1]]
    }
    gam_prep$sp <- sp
    gam_prep$pP$sp[1] <- sp
  } 
  ret <- map2(
    index_list, eval_list, 
    ~ purrr::possibly(fit_ml_once, 
                      quiet = FALSE,
                      otherwise = list(
                        coef = rep(NA_real_, ncol(spec_object$X)),
                        pve = NA_real_,
                        sp = NA_real_)
                      )(.x, .y, gam_prep = gam_prep, sp = sp)
  )
  names(ret) <- levels(data$id)
  coef <- map(ret, "coef")
  failed <- which(map_lgl(coef, ~ any(is.na(.))))
  if (length(failed) > 0) {
    stop("Basis representation failed for entries:\n ", 
         paste(unname(failed), collapse = ", "))
  }
  list(coef = coef, 
       pve = map_dbl(ret, "pve"), 
       sp = if (penalized & !fixed_sp) map_dbl(ret, "sp") else NULL)
}


fit_ml_once <- function(index, evaluations, gam_prep, sp) {
  G_tmp <- gam_prep
  G_tmp$X <- G_tmp$X[index, ]
  G_tmp$y <- evaluations
  G_tmp$n <- length(evaluations)
  G_tmp$w <- rep(1, G_tmp$n)
  mf <- data.frame(x = G_tmp$y)
  mf$X <- G_tmp$X
  attributes(mf) <- attributes(G_tmp$mf )
  G_tmp$mf <- mf
  G_tmp$offset <- rep(0, G_tmp$n)
  m <- gam(G = G_tmp, family = G_tmp$family) 
  list(coef = unname(m$coefficients),
       pve = (m$null.deviance - m$deviance)/m$null.deviance,
       # FIXME: null deviance not always defined..? (Gamma(link = inverse))
       sp = m$sp)
}

if (FALSE) {
  # fit_penalized_global <- function(data, spec_object, gam_args) {
  #   # by = id:   does not work due to centering per
  #   # bs = "fs": does not work because of internal nat.param
  #   
  #   data$id <- factor(data$id, ordered = FALSE)
  #   # create formula
  #   s_term <- spec_object$call
  #   if (!is.null(s_term$xt)) {
  #     warning("basis specification argument `xt` is ignored if `global = TRUE`.")
  #   }
  #   s_term$xt <- s_term$bs
  #   s_term$bs <- "fs"
  #   s_term[[length(s_term) + 1]] <- as.symbol("id")
  #   s_formula <- data ~ arg - 1
  #   s_formula[[3]][[2]] <- s_term
  #   # can't use "s(..., by = id") because of centering.
  #   # set penalty on null space components of "fs" to be negligible:
  #   gam_args$sp <- c(rep(1e-9, spec_object$null.space.dim), gam_args$sp)
  #   m <- do.call(bam,
  #                c(list(formula = s_formula, data = data), gam_args))
  #   browser()
  #   coef_list <- split(m$coefficients, 
  #                      rep(levels(data$id), 
  #                          each = spec_object$bs.dim))[levels(data$id)]
  #   null_deviance <- map_dbl(split(m$y, data$id)[levels(data$id)], 
  #     ~ sum(m$family$dev.resids(.x, mean(.x), 1))
  #     )
  #   fit_deviance <- map2_dbl(
  #     split(m$y, data$id)[levels(data$id)],
  #     split(m$family$linkinv(m$fitted), data$id)[levels(data$id)], 
  #     ~ sum(m$family$dev.resids(.x, .y, 1))
  #     )
  #   pve <- (null_deviance - fit_deviance)/null_deviance
  #   return(list(coef = coef_list, pve = pve))
  # }
}
  
