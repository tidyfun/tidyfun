new_tfb_wavelet <- function(data, domain = NULL, level = 2, verbose = TRUE,
                            resolution = NULL, filter_number = 5, ...) {
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
  
  
  
  # wd_args <- list(...)[names(list(...)) %in% 
  #                        names(formals(wavethresh::wd))]
  # 
  # threshold_args <- list(...)[names(list(...)) %in% 
  #                               names(formals(wavethresh::threshold.wd))]
  # threshold_args$levels <- level
  # if ("type" %in% names(list(...))) {
  #   wd_args$type <- NULL
  #   if (threshold_args$type %in% c("wavelet", "station")) {
  #     threshold_args$type <- NULL
  #     warning("type only refers to threshold.wd(type)")
  #   }
  # }
  
  
  # fit <- fit_wavelet(data, threshold_args, wd_args, arg_u, regular)
  
  # n_levels_wd <- nlevelsWT(fit$wd_coefs[[1]]) - 1
  
  
  X <- cbind(1, ZDaub(arg_u$x,
                      numLevels = level,
                      filterNumber = filter_number,
                      resolution = 16384
  ))
  
  fit <- fit_wavelet_matrix(data, Z = X)
  
  
  basis_constructor <- function(arg = arg) {
    predict_matrix(X = X, arg_old = unname(unlist(arg_u)), arg_new = arg)
  }
  
  ret <- structure(coefs,
                   domain = domain,
                   # thresh_arg = formals(wavethresh::threshold.wd),
                   # wd_arg = formals(wavethresh::wd),
                   basis_args = list(level = level, 
                                     filter_number = filter_number),
                   basis = memoise(basis_constructor),
                   basis_matrix = X,
                   resolution = resolution,
                   # filter = fit$wd_coefs[[1]]$filter,
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
