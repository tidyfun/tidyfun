#' @importFrom wavethresh wd wr.wd putC.wd
new_tfb_wavelet <- function(data, domain = NULL, level = 2, verbose = TRUE,
                            resolution = NULL, filter_number = 5, 
                            penalized = FALSE, ...) {
  
  assert_numeric(level, lower = 2, upper = 10)
  assert_numeric(filter_number, lower = 1, upper = 10)
  assert_flag(penalized)
  
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
  
  # Use names from glmnet, because cv.glmnet uses less inputs glmnet, but can 
  # use glmnet arguments
  if (penalized) {
    glmnet_args <- list(...)[names(list(...)) %in% names(formals(
      glmnet::glmnet))] 
    if (!any(grepl("nlambda|lambda", names(glmnet_args)))) 
      glmnet_args$nlambda <- 100
    if (!"nfolds" %in% names(glmnet_args))
      glmnet_args$nfolds <- 10
    if (!"alpha" %in% names(glmnet_args))
      glmnet_args$alpha <- 1
    if (!"family" %in% names(glmnet_args))
      glmnet_args$family <- "gaussian"
    # intercept needs to be TRUE
    if ("intercept" %in% names(glmnet_args)) {
      if (!glmnet_args$intercept) {
        glmnet_args$intercept <- TRUE
        warning("The intercept must always be included. Setting intercept=TRUE")
      }
    }     
  } else {
    glmnet_args <- NULL
  }
  
  X <- ZDaub(interp_index,
             numLevels = level,
             filterNumber = filter_number,
             resolution = 16384)
  
  X <- predict_matrix(X, interp_index, arg_u$x)
  
  if (regular) {
    fit <- fit_wavelet(data, Z = X, penalized = penalized,
                       glmnet_args) 
  } # else {
  #   fit <- fit_wavelet_irr(data, Z = X, penalized = penalized,
  #                          glmnet_args)
  # }
  
  X <- cbind(1, X, arg_u$x)
  
  basis_constructor <- function(arg = arg) {
    predict_matrix(X = X, arg_old = unname(unlist(arg_u)), arg_new = arg)
  }
  
  basis_label <- paste0(filter_number, " Vanishing Moments, eval to level ",
                        level, ", Lasso: ", penalized)
  
  ret <- structure(fit,
                   domain = domain,
                   glmnet_args = glmnet_args,
                   basis_args = list(level = level, 
                                     filter_number = filter_number),
                   basis_label = basis_label,
                   basis = memoise(basis_constructor),
                   basis_matrix = X,
                   resolution = resolution,
                   arg = arg_u$x,
                   class = c("tfb_wavelet", "tfb", "tf")
  )
  ret
}

#' Wavelet-based representation of functional data
#' 
#' Represent curves with wavelet bases
#' 
#' The `level` and `filter_number` arguments define the wavelet matrix. If 
#' `filter_number = 1` the matrix represents the Haar Wavelet, if 
#' `filter_number > 1` Daubechies "extremal phase" wavelets will be used 
#' (see [wavethresh::filter.select()]). The `level` parameter defines up to which 
#' level the wavelet is evaluated. The higher the `level` the bigger your output
#' and the higher the variability in your curves.
#' 
#' If `penalized = FALSE` (the default), the coefficients will be estimated 
#' using a least squares, if `penalized = TRUE` it uses lasso regression with 
#' cross-validation [glmnet::cv.glmnet]. The default is `nlambda = 100`.
#' 
#' @inheritParams tfb
#' @param level The level to which the wavelet is evaluated. Defined for 2 to 10.
#' @param filter_number The number of vanishing moments for the wavelet. Higher
#' numbers mean the wavelet has more variability. Possible Inputs 1 to 10.
#' @param penalized logical; if `FALSE` a least squares fit will be performed. 
#' If `TRUE` [glmnet::cv.glmnet] will be used for the fit.
#' @param ... Only used if `penalized = TRUE`. Arguments for 
#' [glmnet::cv.glmnet]. The default is Lasso-Regression with `nlambda = 100`.
#' @return a `tfb`-object
#' @references https://projecteuclid.org/euclid.ejs/1323785605#supplemental
#' @author Sven Lorenz
tfb_wavelet <- function(data, ...) UseMethod("tfb_wavelet")

#' @export
#' @inheritParams tfd.data.frame
#' @describeIn tfb_spline convert data frames
tfb_wavelet.data.frame <- function(data, id = 1, arg = 2, value = 3,
                                   domain = NULL, level = 2, verbose = TRUE,
                                   resolution = NULL, filter_number = 5, 
                                   penalized = FALSE, ...) {
  data <- df_2_df(data, id, arg, value)
  ret <- new_tfb_wavelet(data,
                         domain = domain, level = level,
                         verbose = verbose, resolution = resolution, 
                         filter_number = filter_number, 
                         penalized = penalized, ...)
  assert_arg(tf_arg(ret), ret)
  ret
}

#' @export
#' @describeIn tfb_spline convert matrices
tfb_wavelet.matrix <- function(data, arg = NULL, domain = NULL, verbose = TRUE, 
                               resolution = NULL, level = 2,
                               filter_number = 5, penalized = FALSE, ...) {
  arg <- unlist(find_arg(data, arg))
  data_names <- rownames(data)
  data <- mat_2_df(data, arg)
  ret <- new_tfb_wavelet(data, domain = domain, level = level,
                         verbose = verbose, resolution = resolution, 
                         filter_number = filter_number, 
                         penalized = penalized, ...)
  names(ret) <- data_names
  assert_arg(tf_arg(ret), ret)
  ret
}

#' @export
#' @describeIn tfb_spline convert `tfd` (raw functional data)
tfb_wavelet.tfd <- function(data, arg = NULL, domain = NULL, 
                            verbose = TRUE, resolution = NULL, 
                            level = 2, filter_number = 5, 
                            penalized = FALSE, ...) {
  arg <- arg %||% tf_arg(data)
  domain <- domain %||% tf_domain(data)
  resolution <- resolution %||% tf_resolution(data)
  names_data <- names(data)
  data <- as.data.frame(as.data.frame(data, arg))
  ret <- tfb_wavelet(data, domain = domain, resolution = resolution,
                     level = level, filter_number = filter_number,
                     penalized = penalized,
                     verbose = verbose, ...)
  names(ret) <- names_data
  ret
}


#' @export
#' @describeIn tfb_spline convert `tfb`: modify wavelet basis or Lasso args.
tfb_wavelet.tfb <- function(data, domain = NULL, level = 2,
                            verbose = TRUE, arg = NULL,
                            resolution = NULL, filter_number = 5, 
                            penalized = FALSE, ...) {
  arg <- arg %||% tf_arg(data)
  resolution <- resolution %||% tf_resolution(data)
  domain <- domain %||% tf_domain(data)
  
  
  glmnet_args <- modifyList(
    as.list(attr(data, "glmnet_args")),
    list(...)[names(list(...)) %in% names(formals(glmnet::glmnet))]
  )
  
  glmnet_args <- c(glmnet_args, 
                   list(...)[!names(list(...)) %in% names(glmnet_args)])
  
  
  names_data <- names(data)
  data <- as.data.frame(as.data.frame(data, arg = arg))
  ret <- do.call("tfb_wavelet", c(list(data), domain = domain, level = level,
                                  verbose = verbose, 
                                  resolution = resolution, 
                                  filter_number = filter_number,
                                  penalized = penalized,
                                  glmnet_args, ...))
  names(ret) <- names_data
  ret
}
