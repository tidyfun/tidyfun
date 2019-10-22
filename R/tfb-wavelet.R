
new_tfb_wavelet <- function(data, wavelet = c("DWT", "NDWT", "WPT", "NWPT", "MWD"),
                            filter_number = NULL, family = NULL, 
                            prefilter_type = NULL, filter_type = NULL,
                            bc = NULL, type = NULL, min_scale = 0, precond = NULL,
                            finish_level = NULL,
                            verbose = TRUE, ...) {
  
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
#' DWT: [wavethresh:wd(type = "wavelet")]
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

tfb_wavelet.data.frame <- function() {
  
}


tfb_wavelet.matrix <- function() {
  
}

tfb_wavelet.tfd <- function() {
  
}

tfb_wavelet.tfb <- function() {
  
}
