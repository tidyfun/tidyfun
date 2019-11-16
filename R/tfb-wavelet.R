new_tfb_wavelet <- function(data, domain = NULL, levels = 2, verbose = TRUE,
                            resolution = NULL, ...) {
  domain <- domain %||% range(data$arg)
  arg_u <- mgcv::uniquecombs(data$arg, ordered = TRUE)
  resolution <- resolution %||% get_resolution(arg_u)
  domain <- c(
    round_resolution(domain[1], resolution, -1),
    round_resolution(domain[2], resolution, 1)
  )
  
  # explicit factor-conversion to avoid reordering:
  data$id <- factor(data$id, levels = unique(as.character(data$id)))
  
  n_evaluations <- table(data$id)
  arg_list <- split(data$arg, data$id)
  regular <- all(duplicated(arg_list)[-1])
  
  if (regular) {
    dyadic_params <- check_dyadic(nrow(arg_u))
    spacing_params <- check_spacing(sort(unique(data$arg)))
    data <- grid_adjustment(data, dyadic_params, spacing_params)
  } else {
    dyadic_params <- check_dyadic(n_evaluations)
    # check_spacing needs to handle lists to make the output similar to check_dyadic
    spacing_params <- lapply(arg_list, check_spacing)
    data <- grid_adjustment(data, dyadic_params, spacing_params)
  }
  
  
  
  wd_args <- list(...)[names(list(...)) %in% 
                         names(formals(wavethresh::wd))]
  
  threshold_args <- list(...)[names(list(...)) %in% 
                                names(formals(wavethresh::threshold.wd))]
  threshold_args$levels <- levels
  if ("type" %in% names(list(...))) {
    wd_args$type <- NULL
    if (threshold_args$type %in% c("wavelet", "station")) {
      threshold_args$type <- NULL
      warning("type only refers to threshold.wd(type)")
    }
  }
  
  
  fit <- fit_wavelet(data, threshold_args, wd_args, arg_u, regular)
  
  n_levels_wd <- nlevelsWT(fit$wd_coefs[[1]]) - 1
  
  X <- cbind(1, ZDaub(arg_u$x,
                      numLevels = n_levels_wd,
                      filterNumber = fit$wd_coefs[[1]]$filter$filter.number,
                      resolution = 16384
  ))
  
  coefs <- lapply(fit$wd_coefs, function(x) {
    c(tail(x$C, 1), x$D)[1:n_levels_wd^2]
  })
  
  basis_constructor <- function(arg = arg) {
    predict_matrix(X = X, arg_old = unname(unlist(arg_u)), arg_new = arg)
  }
  
  ret <- structure(coefs,
                   domain = domain,
                   thresh_arg = formals(wavethresh::threshold.wd),
                   wd_arg = formals(wavethresh::wd),
                   basis = memoise(basis_constructor),
                   basis_matrix = X,
                   resolution = resolution,
                   filter = fit$wd_coefs[[1]]$filter,
                   arg = arg_u$x,
                   class = c("tfb_wavelet", "tfb", "tf")
  )
  ret
}


#' @param data A data.frame, matrix or tf-object.
#' @param wavelet Every input corresponds with a wavelet function of the
#' wavethresh-package, since they have different inputs it is advised to look at
#' the help pages:
#' DWT: [wavethresh::wd(type = "wavelet")]
#' NDWT: [wavethresh:wd(type = "station")]
#' WPT: [wavethresh:wp()]
#' NWPT: [wavethresh:wpst()]
#' MWD: [wavethresh:mwd()]
#' @param filter_number Number of vanishing moments.
#' @param family
#' @param ... Arguments for thresholding. For possible Arguments look at the
#' help pages of wavethresh::threshold.your_wavelet_method.
#' @return a `tfb`-object
tfb_wavelet <- function() UseMethod("tfb_wavelet")

tfb_wavelet.data.frame <- function(data, id = 1, arg = 2, value = 3,
                                   domain = NULL, levels = 2, verbose = TRUE,
                                   ...) {
  data <- df_2_df(data, id, arg, value)
  ret <- new_tfb_wavelet(data,
                         domain = domain, levels = levels,
                         verbose = TRUE, ...
  )
  assert_arg(tf_arg(ret), ret)
  ret
}


tfb_wavelet.matrix <- function() {
  
}

tfb_wavelet.tfd <- function() {
  
}

tfb_wavelet.tfb <- function() {
  
}
