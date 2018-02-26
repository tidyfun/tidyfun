#' @import mgcv
smooth_spec_wrapper <- function(spec) {
  function(argvals) {
    mgcv::Predict.matrix(object = spec, data = data.frame(argvals = argvals))
  }
} 

#' @importFrom stats var na.omit median
new_fbase <- function(data, regular, domain = NULL,   
    penalized = TRUE, signif = 4, verbose = TRUE, ...) {
  data$argvals <- .adjust_resolution(data$argvals, signif, unique = FALSE)
  argvals_u <- mgcv::uniquecombs(data$argvals, ordered = TRUE)
  domain <- domain %||% range(argvals_u)
  s_args <- list(...)[names(list(...)) %in% names(formals(mgcv::s))]
  if (!("bs" %in% names(s_args))) s_args$bs <- "cr"
  if (!("k" %in% names(s_args))) s_args$k <- min(15, nrow(argvals_u))
  magic_args <- list(...)[names(list(...)) %in% names(formals(mgcv::magic))]
  if (!("sp" %in% names(magic_args))) magic_args$sp <- -1
  s_call <- as.call(c(quote(s), quote(argvals), s_args))
  s_spec <- eval(s_call)
  if ("deriv" %in% names(list(...))) s_spec$deriv <- list(...)$deriv
  if ("mono" %in% names(list(...))) s_spec$mono <- list(...)$mono
  spec_object <- smooth.construct(s_spec, 
    data = data.frame(argvals = argvals_u$x), knots = NULL)
  n_evaluations <- table(data$id)
  underdet <- n_evaluations < spec_object$bs.dim
  eval_list <- split(data$data, data$id)
  if (!penalized) {
    if (any(underdet) & verbose) {
      warning("More basis functions than evaluations.", 
        " Interpolation will be unreliable.")
    }
    if (regular) {
      eval_matrix <- do.call(cbind, eval_list)
      qr_basis <- qr(spec_object$X)
      coef_list <- qr.coef(qr = qr_basis, y = eval_matrix)
      coef_list <- split(coef_list, col(coef_list))
      pve <- 1 - apply(qr.resid(qr = qr_basis, 
        y = eval_matrix), 2, var)/apply(eval_matrix, 2, var)
    } else {
      index_list <- split(attr(argvals_u, "index"), data$id)
      coef_list <- map2(index_list, eval_list, 
        ~ qr.coef(qr=qr(spec_object$X[.x,]), y = .y))
      pve <- unlist(map2(index_list, eval_list, 
        ~ 1 - var(qr.resid(qr=qr(spec_object$X[.x,]), y = .y))/var(.y)))
    }
    # need to remove NAs if dim(basis) > length(argvals)
    coef_list[underdet] <- map(coef_list[underdet], na_to_0) 
  } else {
    index_list <- split(attr(argvals_u, "index"), data$id)
    coef_list <- map2(index_list, eval_list, 
      ~ magic_smooth_coef(.y, .x, spec_object, magic_args))
    pve <- unlist(map(coef_list, 2))
    coef_list <- map(coef_list, 1)
  }
  if (verbose) {
    message("Percentage of raw input data variance preserved in basis representation:\n", 
      "(per functional observation, approx.):")
    print(summary(round(100 * pve, 1)))
  }
  names(coef_list) <- names(coef_list) %||% seq_along(coef_list)
  basis_constructor <- smooth_spec_wrapper(spec_object)
  structure(coef_list, 
    domain = domain,
    basis = memoise(basis_constructor),
    basis_label = deparse(s_call, width.cutoff = 60)[1],
    basis_args = s_args,
    basis_matrix = spec_object$X,
    argvals = argvals_u$x,
    signif_argvals = signif, 
    class = c("fbase", "fvector"))
}

magic_smooth_coef <- function(evaluations, index, spec_object, magic_args) {
  m <- do.call(mgcv::magic, 
    c(list(y = evaluations, X = spec_object$X[index,], S = spec_object$S), 
      flatten(list(off = 1, magic_args))))
  list(coef = m$b, pve = 1 - m$scale/var(evaluations))
}

#-------------------------------------------------------------------------------
#' Constructors for (smoothed) functional data in basis representation
#' 
#' Various constructor and conversion methods.
#'
#' `fbase` takes the data it is supplied with and tries to represent them as linear
#' combinations of a set of common spline basis functions identical for all
#' observations with coefficient vectors estimated for each observation. The
#' basis used is set up via a call to [mgcv::s()] and all the spline bases
#' discussed in [mgcv::smooth.terms] are available, in principle. Depending on
#' the value of the `penalized`-flag, the coefficient vectors for each
#' observation are then estimated via fitting a small GAM for each observation
#' via [mgcv::magic()] or via simple ordinary least squares.
#'
#' After the "smoothed" representation is computed, the amount of smoothing that
#' was performed is reported in terms of the "percentage of variance preserved",
#' which is the variance of the smoothed function values divided by the variance
#' of the original values. The `...` arguments supplies arguments to both the
#' spline basis set up (via [mgcv::s()]) and the estimation (via
#' [mgcv::magic()]), most important are probably how many basis functions `k`
#' the spline basis should have and/or manually setting the smoothing parameter
#' `sp`. The latter can also be used to enforce the same amount of penalization
#' for all functional observations.
#' 
#' @param data a `matrix`, `data.frame` or `list` of suitable shape, or another `fvector`-object.
#' @return an `fbase`-object (or a `data.frame`/`matrix` for the conversion functions, obviously.)
#' @rdname fbase
#' @export
fbase <- function(data, ...) UseMethod("fbase")


#' @export
#' @inheritParams feval.data.frame
#' @param penalized should the coefficients of the basis representation be estimated
#'   via [mgcv::magic()] (default) or ordinary least squares.
#' @param ... arguments to the calls to [mgcv::s()] setting up the basis and
#'   [mgcv::magic()] (if `penalized` is TRUE). If not user-specified here,
#'   `tidyfun` uses `k=15` cubic regression spline basis functions (i.e., `bs =
#'   "cr"`) by default, but at least how many basis functions `k` the spline
#'   basis should have probably needs to be set manually ....
#' @rdname fbase
#' @export
fbase.data.frame <- function(data, id = 1, argvals = 2, value = 3,  
    domain = NULL,   penalized = TRUE, signif = 4, ...) {
  data <- na.omit(data[, c(id, argvals, value)])
  colnames(data) <- c("id", "argvals", "data")
  stopifnot(nrow(data) > 0, 
    is.numeric(data[[2]]), 
    is.numeric(data[[3]]))
  regular <- n_distinct(table(data[[1]])) == 1
  new_fbase(data, regular, domain = domain,   
    penalized = penalized, signif = signif, ...)
}

#' @rdname fbase
#' @export
fbase.matrix <- function(data, argvals = NULL, 
  domain = NULL,   penalized = TRUE, signif = 4, ...) {
  stopifnot(is.numeric(data))
  argvals <- unlist(find_argvals(data, argvals))
  id <- make.unique(rownames(data) %||% as.character(seq_len(dim(data)[1])))
  data <- na.omit(data_frame(id = id[row(data)], argvals = argvals[col(data)], 
    data = as.vector(data)))
  regular <- n_distinct(table(data[[1]])) == 1
  new_fbase(data, regular,  domain = domain,   
    penalized = penalized, signif = signif, ...)
}
#' @rdname fbase
#' @export
fbase.numeric <- function(data, argvals = NULL, 
  domain = NULL,   penalized = TRUE, signif = 4, ...) {
  data <- t(as.matrix(data))
  fbase(data = data, argvals = argvals, domain = domain, penalized = penalized,
    signif = signif, ...)
}


#' @rdname fbase
#' @export
fbase.list <- function(data, argvals = NULL,  
  domain = NULL,   penalized = TRUE, signif = 4, ...) {
  vectors <- sapply(data, is.numeric)
  stopifnot(all(vectors) | !any(vectors))
  if (all(vectors)) {
    lengths <- sapply(data, length)
    if (all(lengths == lengths[1])) {
      data <- do.call(rbind, data)
      #dispatch to matrix method
      args <- list(data, argvals,  domain = domain,   
        penalized = penalized, signif = signif, ...)
      return(do.call(fbase, args))
    } else {
      stopifnot(!is.null(argvals), length(argvals) == length(data), 
        all(sapply(argvals, length) == lengths))
      data <- map2(argvals, data, ~as.data.frame(cbind(argvals = .x, data = .y)))
    }
  }
  dims <- map(data, dim)
  stopifnot(all(sapply(dims, length) == 2), all(map(dims, ~.x[2]) == 2),
    all(rapply(data, is.numeric)))
  data <- data_frame(id = make.unique(names(data) %||% seq_along(data)), 
      funs = data) %>% tidyr::unnest
  #dispatch to data.frame method
  fbase(data, basis = basis, domain = domain,   
    penalized = penalized, signif = signif, ...)
}

#' @rdname fbase
#' @export
fbase.feval <- function(data, argvals = NULL, 
  domain = NULL, penalized = TRUE, signif = 4, ...) {
  argvals <- argvals %||% argvals(data)
  domain <- domain %||% domain(data)
  data <- as.data.frame(data, argvals)
  fbase(data, basis = basis, domain = domain,   
    penalized = penalized, signif = signif, ...)
}

#' @rdname fbase
#' @export
fbase.fbase <- function(data, argvals = NULL,
  domain = NULL, penalized = TRUE, signif = 4, ...) {
  argvals <- argvals %||% argvals(data)
  domain <- domain %||% domain(data)
  s_args <- modifyList(attr(data, "basis_args"),
    list(...)[names(list(...)) %in% names(formals(mgcv::s))])
  data <- as.data.frame(data, argvals = argvals)
  do.call("fbase", c(list(data), basis = basis, domain = domain,
    penalized = penalized, signif = signif, s_args))
}
