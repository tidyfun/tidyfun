new_tfb_wavelet <- function(data, domain = NULL, levels = 6, verbose = TRUE, ...) {
  
  domain <- domain %||% range(data$arg)
  n <- domain[2]
  
  dyadic_params <- check_dyadic(n)
  spacing_params <- check_spacing(sort(unique(data$arg)))
  # Use Signal Extension and grid adjustment algorithms
  data <- grid_adjustment(data, dyadic_params, spacing_params)
  
  
  threshold_args <- list(...)[names(list(...)) %in% 
                                names(formals(wavethresh::threshold.wd))]
  wd_args <- list(...)[names(list(...)) %in% names(formals(wavethresh::wd))]
  
  fit <- fit_wavelet(data, threshold_args, wd_args, levels = levels)
  
  ret <- structure(fit,
                   
                   domain = domain,
                   filter = filter,
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
                                   domain = NULL, levels = 6, verbose = TRUE,
                                   ...) {
  data <- df_2_df(data, id, arg, value)
  ret <- new_tfb_wavelet(data, domain = domain, levels = levels, 
                         verbose = TRUE, ...)
  assert_arg(tf_arg(ret), ret)
  ret
}


tfb_wavelet.matrix <- function() {
  
}

tfb_wavelet.tfd <- function() {
  
}

tfb_wavelet.tfb <- function() {
  
}
