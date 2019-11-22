new_tfb_wavelet <- function(data, domain = NULL, level = 2, verbose = TRUE,
                            resolution = NULL, filter_number = 5, 
                            least_squares = TRUE, ...) {
  domain <- domain %||% range(data$arg)
  arg_u <- mgcv::uniquecombs(data$arg, ordered = TRUE)
  resolution <- resolution %||% get_resolution(arg_u)
  domain <- c(
    round_resolution(domain[1], resolution, -1),
    round_resolution(domain[2], resolution, 1)
  )
  
  # explicit factor-conversion to avoid reordering:
  data$id <- factor(data$id, levels = unique(as.character(data$id)))
  
  arg_list <- split(data$arg, data$id)
  regular <- all(duplicated(arg_list)[-1])
  
  if (regular) {
    interp_index <- interpolate_arg(arg_list = list(arg_u$x))
  } #else {
  #   interp_index <- interpolate_arg(arg_list = arg_list)
  # }
  
  
  X <- ZDaub(interp_index,
             numLevels = level,
             filterNumber = filter_number,
             resolution = 16384)
  
  X <- scale(predict_matrix(X, interp_index, arg_u$x), center = FALSE)
  
  fit <- fit_wavelet_matrix(data, Z = X, least_squares = TRUE)
  
  X <- cbind(1, X)
  
  basis_constructor <- function(arg = arg) {
    predict_matrix(X = X, arg_old = unname(unlist(arg_u)), arg_new = arg)
  }
  
  ret <- structure(fit$fit,
                   domain = domain,
                   basis_args = list(level = level, 
                                     filter_number = filter_number),
                   basis = memoise(basis_constructor),
                   basis_matrix = X,
                   resolution = resolution,
                   slope_params = fit$slope_params,
                   arg = arg_u$x,
                   class = c("tfb_wavelet", "tfb", "tf")
  )
  ret
}


#' @param data A data.frame, matrix or tf-object.
#' @param domain range of the `arg`.
#' @param level The resolution level of the wavelet.
#' @param resolution resolution of the evaluation grid. See details for [tfd()].
#' @param family
#' @param ... Arguments for [wavethresh::wd] and [wavethresh::threshold.wd]. 
#' `type` will only be handled by [wavethresh::threshold.wd].
#' @return a `tfb`-object
tfb_wavelet <- function(data, ...) UseMethod("tfb_wavelet")

#' @export
#' @inheritParams tfd.data.frame
#' @describeIn tfb_spline convert data frames
tfb_wavelet.data.frame <- function(data, id = 1, arg = 2, value = 3,
                                   domain = NULL, level = 2, verbose = TRUE,
                                   resolution = NULL, filter_number = 5, ...) {
  data <- df_2_df(data, id, arg, value)
  ret <- new_tfb_wavelet(data,
                         domain = domain, level = level,
                         verbose = verbose, resolution = resolution, ...)
  assert_arg(tf_arg(ret), ret)
  ret
}


# tfb_wavelet.matrix <- function(data, domain = NULL, level = 2,
#                                verbose = TRUE, arg = NULL,
#                                resolution = NULL, filter_number = 5, ...) {
#   arg <- unlist(find_arg(data, arg))
#   data_names <- rownames(data)
#   data <- mat_2_df(data, arg)
#   ret <- new_tfb_wavelet(data, domain = domain, level = level,
#                          verbose = verbose, resolution = resolution, ...)
#   names(ret) <- data_names
#   assert_arg(tf_arg(ret), ret)
#   ret
# }

tfb_wavelet.tfd <- function(data, domain = NULL, level = 2,
                            verbose = TRUE, arg = NULL, 
                            resolution = NULL, filter_number = 5, ...) {
  arg <- arg %||% tf_arg(data)
  domain <- domain %||% tf_domain(data)
  resolution <- resolution %||% tf_resolution(data)
  names_data <- names(data)
  data <- as.data.frame(data, arg)
  ret <- tfb_wavelet(data, domain = domain, level = level,
                     verbose = verbose, ...)
  names(ret) <- names_data
  ret
}

# tfb_wavelet.tfb <- function(data, domain = NULL, level = 2,
#                             verbose = TRUE, arg = NULL, 
#                             resolution = NULL, filter_number = 5, ...) {
#   arg <- arg %||% tf_arg(data)
#   resolution <- resolution %||% tf_resolution(data)
#   domain <- domain %||% tf_domain(data)
#   wd_args <- modifyList(
#     attr(data, "wd_arg"),
#     list(...)[names(list(...)) %in% names(formals(wavethresh::wd))]
#   )
#   threshold_args <- modifyList(
#     attr(data, "thresh_arg"),
#     list(...)[names(list(...)) %in% names(formals(wavethresh::threshold.wd))]
#   )
#   wd_args$data <- NULL
#   wd_args$verbose <- NULL
#   threshold_args$verbose <- NULL
#   threshold_args$value <- NULL
#   names_data <- names(data)
#   data <- as.data.frame(data, arg = arg)
#   ret <- do.call("tfb_wavelet", c(list(data), domain = domain, level = level,
#                                   verbose = verbose, arg = arg, 
#                                   resolution = resolution, 
#                                   wd_args, threshold_args, ...))
#   names(ret) <- names_data
#   ret
# }
