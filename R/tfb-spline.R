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
  magic_args <- list(...)[names(list(...)) %in% names(formals(mgcv::magic))]
  if (!("sp" %in% names(magic_args))) magic_args$sp <- -1
  s_call <- as.call(c(quote(s), quote(arg), s_args))
  s_spec <- eval(s_call)
  spec_object <- smooth.construct(s_spec, data = data.frame(arg = arg_u$x), 
                                  knots = NULL)
  
  n_evaluations <- table(data$id)
  arg_list <- split(data$arg, data$id)
  regular <- all(duplicated(arg_list)[-1])
  underdet <- n_evaluations <= spec_object$bs.dim
  
  eval_list <- split(data$data, data$id)
  if (!penalized) {
    if (any(underdet)) {
      stop("At least as many basis functions as evaluations for ",
        sum(underdet), " functions.",
        " Use penalized = TRUE or reduce k for spline interpolation."
      )
    }
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
  } else {
    index_list <- split(attr(arg_u, "index"), data$id)
    coef_list <- map2(
      index_list, eval_list,
      ~magic_smooth_coef(.y, .x, spec_object, magic_args)
    )
    pve <- unlist(map(coef_list, 2))
    coef_list <- map(coef_list, 1)
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
    print(summary(round(100 * pve, 1)))
  }
  names(coef_list) <- levels(data$id)
  basis_constructor <- smooth_spec_wrapper(spec_object)
  ret <- structure(coef_list,
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
  data <- data_frame(
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
