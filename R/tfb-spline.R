#' @importFrom stats var na.omit median
new_tfb_spline <- function(data, domain = NULL, penalized = TRUE, 
                           resolution = NULL, verbose = TRUE, ...) {
  domain <- domain %||% range(data$arg)
  arg_u <- mgcv::uniquecombs(data$arg, ordered = TRUE)
  resolution <- resolution %||% get_resolution(arg_u)
  # explicit factor-conversion to avoid reordering:
  data$id <- factor(data$id, levels = unique(as.character(data$id)))
  
  s_args <- list(...)[names(list(...)) %in% names(formals(mgcv::s))]
  if (!("bs" %in% names(s_args))) s_args$bs <- "cr"
  if (!("k" %in% names(s_args))) s_args$k <- min(25, nrow(arg_u))
  gam_args <- list(...)[names(list(...)) %in% names(formals(mgcv::gam))]
  if (!("sp" %in% names(gam_args))) gam_args$sp <- -1
  s_call <- as.call(c(quote(s), quote(arg), s_args))
  s_spec <- eval(s_call)
  spec_object <- smooth.construct(s_spec, data = data.frame(arg = arg_u$x), 
                                  knots = NULL)
  
  n_evaluations <- table(data$id)
  arg_list <- split(data$arg, data$id)
  regular <- all(duplicated(arg_list)[-1])
  ls_fit <- is.null(gam_args$family) || 
    grepl("gaussian", deparse(gam_args$family))
  if (!penalized) {
    underdetermined <- n_evaluations <= spec_object$bs.dim
    if (any(underdetermined)) {
      stop("At least as many basis functions as evaluations for ",
        sum(underdetermined), " functions.",
        " Use penalized = TRUE or reduce k for spline interpolation.")
    }
   fit <- 
     fit_unpenalized(data = data, spec_object = spec_object, arg_u = arg_u, 
                     gam_args = gam_args, regular = regular, ls_fit = ls_fit)
  } else {
    fit <- fit_penalized(data = data, spec_object = spec_object, arg_u = arg_u,
                         gam_args = gam_args, regular = regular, global = global,
                         ls_fit = ls_fit)
  }
  if (!regular) {
    arg_u <- data.frame(x = unique(round_resolution(arg_u$x, resolution)))
    spec_object <- smooth.construct(s_spec, data = data.frame(arg = arg_u$x), 
                                    knots = NULL)
  }
  if (verbose) {
    message(
      "Percentage of raw input data variance preserved in basis representation:\n",
      "(per functional observation, tf_approx.):"
    )
    print(summary(round(100 * fit$pve, 1)))
  }
  
  basis_constructor <- smooth_spec_wrapper(spec_object)
  ret <- structure(fit[["coef"]],
                   domain = domain,
                   basis = memoise(basis_constructor),
                   basis_label = deparse(s_call, width.cutoff = 60)[1],
                   basis_args = s_args,
                   basis_matrix = spec_object$X,
                   arg = arg_u$x,
                   resolution = resolution,
                   class = c("tfb_spline", "tfb", "tf")
  )
  ret
}


#-------------------------------------------------------------------------------

#' Spline-based representation of functional data
#' 
#' Represent curves as a weighted sum of spline basis functions.
#' 
#' The basis to be used is set up via a call to [mgcv::s()] and all the spline bases
#' discussed in [mgcv::smooth.terms()] are available, in principle. Depending on
#' the value of the `penalized`-flag, the coefficient vectors for each
#' observation are then estimated via fitting a (small) GAM for each observation
#' via [mgcv::magic()] or via simple ordinary least squares.
#'
#' After the "smoothed" representation is computed, the amount of smoothing that
#' was performed is reported in terms of the "percentage of variance preserved",
#' which is the variance of the smoothed function values divided by the variance
#' of the original values. The `...` arguments supplies arguments to both the
#' spline basis set up (via [mgcv::s()]) and the estimation (via
#' [mgcv::magic()]), most important: how many basis functions `k` the spline
#' basis should have, the default is 25.
#' 
#' @inheritParams tfb
#' @param ...  arguments to the calls to [mgcv::s()] setting up the basis and
#'   [mgcv::magic()] (if `penalized` is TRUE). If not user-specified here,
#'   `tidyfun` uses `k = 25` cubic regression spline basis functions (i.e., `bs =
#'   "cr"`) by default, but this should be set manually
#' @return a `tfb`-object
#' @seealso [mgcv::smooth.terms()] for spline basis options. 
tfb_spline <- function(data, ...) UseMethod("tfb_spline")

#' @export
#' @inheritParams tfd.data.frame
#' @param penalized should the coefficients of the basis representation be estimated
#'   via [mgcv::magic()] (default) or ordinary least squares.
#' @describeIn tfb_spline convert data frames
tfb_spline.data.frame <- function(data, id = 1, arg = 2, value = 3,
                                  domain = NULL, penalized = TRUE, 
                                  resolution = NULL, ...) {
  data <- df_2_df(data, id, arg, value)
  ret <- new_tfb_spline(data, domain = domain, penalized = penalized,
                        resolution = resolution, ...)
  assert_arg(tf_arg(ret), ret)
  ret
}


#' @export
#' @describeIn tfb_spline convert matrices
tfb_spline.matrix <- function(data, arg = NULL,
                              domain = NULL, penalized = TRUE, 
                              resolution = NULL, ...) {
  arg <- unlist(find_arg(data, arg))
  data_names <- rownames(data)
  data <- mat_2_df(data, arg)
  ret <- new_tfb_spline(data, domain = domain, penalized = penalized,
                        resolution = resolution, ...)
  names(ret) <- data_names
  assert_arg(tf_arg(ret), ret)
  ret
}

#' @export
#' @describeIn tfb_spline convert matrices
tfb_spline.numeric <- function(data, arg = NULL,
                               domain = NULL, penalized = TRUE, 
                               resolution = NULL, ...) {
  data <- t(as.matrix(data))
  tfb(data = data, arg = arg, domain = domain, penalized = penalized,
      resolution = resolution, ...)
}


#' @export
#' @describeIn tfb_spline convert lists
tfb_spline.list <- function(data, arg = NULL,
                            domain = NULL, penalized = TRUE, resolution = NULL, ...) {
  vectors <- vapply(data, is.numeric, logical(1))
  stopifnot(all(vectors) | !any(vectors))
  names_data <- names(data)
  if (all(vectors)) {
    lengths <- vapply(data, length, numeric(1))
    if (all(lengths == lengths[1])) {
      data <- do.call(rbind, data)
      # dispatch to matrix method
      return(tfb(data, arg, domain = domain, penalized = penalized, 
                 resolution = resolution, ...))
    } 
    stopifnot(
      !is.null(arg), length(arg) == length(data),
      all(vapply(arg, length, numeric(1)) == lengths)
    )
    data <- map2(arg, data, ~as.data.frame(cbind(arg = .x, data = .y)))
  }
  dims <- map(data, dim)
  stopifnot(
    all(vapply(dims, length, numeric(1)) == 2), all(map(dims, ~.x[2]) == 2),
    all(rapply(data, is.numeric))
  )
  data <- dplyr::tibble(
    id = unique_id(names(data)) %||% seq_along(data),
    funs = data
  ) %>% { tidyr::unnest(.) }
  # dispatch to data.frame method
  ret <- tfb(data, domain = domain, penalized = penalized, 
             resolution = resolution, ...)
  names(ret) <- names_data
  ret
}


#' @export
#' @describeIn tfb_spline convert `tfd` (raw functional data)
tfb_spline.tfd <- function(data, arg = NULL,
                           domain = NULL, penalized = TRUE, resolution = NULL, ...) {
  arg <- arg %||% tf_arg(data)
  domain <- domain %||% tf_domain(data)
  resolution <- resolution %||% tf_resolution(data)
  names_data <- names(data)
  data <- as.data.frame(data, arg)
  ret <- tfb(data,
             domain = domain,
             penalized = penalized, resolution = resolution, ...
  )
  names(ret) <- names_data
  ret
}

#' @export
#' @describeIn tfb_spline convert `tfb`: modify basis representation
tfb_spline.tfb <- function(data, arg = NULL,
                           domain = NULL, penalized = TRUE, resolution = NULL, ...) {
  arg <- arg %||% tf_arg(data)
  resolution <- resolution %||% tf_resolution(data)
  domain <- domain %||% tf_domain(data)
  s_args <- modifyList(
    attr(data, "basis_args"),
    list(...)[names(list(...)) %in% names(formals(mgcv::s))]
  )
  names_data <- names(data)
  data <- as.data.frame(data, arg = arg)
  ret <- do.call("tfb_spline", c(list(data),
                                 domain = domain,
                                 penalized = penalized, resolution = resolution, s_args
  ))
  names(ret) <- names_data
  ret
}
