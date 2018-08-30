# compute derivatives of data rows by finite differences. 
# returns derivatives at interval midpoints
deriv_matrix <- function(data, arg, order) {
  for (i in 1:order) {
    delta <- diff(arg)
    data <- t(diff(t(data))/delta)
    arg <- (arg[-1] + head(arg,-1))/2
  }
  list(data = data, arg = arg)
}

#trapezoidal quadrature
#' @importFrom utils head
quad_trapez <- function(arg, evaluations) {
  c(0, 0.5 * diff(arg) * (head(evaluations, -1) + evaluations[-1]))
}

deriv_tfb_mgcv <- function(expr, order = 1, arg = tidyfun::arg(expr)) {
  #TODO: make this work for iterated application deriv(deriv(fb)) 
  if (!is.null(attr(expr, "basis_deriv"))) 
    stop("Can't derive or integrate previously derived/integrated tf in basis representation")
  s_args <- attr(expr, "basis_args")
  s_call <- as.call(c(quote(s), quote(arg), s_args))
  s_spec <- eval(s_call)
  spec_object <- smooth.construct(s_spec, 
    data = data.frame(arg =  arg), knots = NULL)
  eps <- min(diff(arg)) / 1000
  basis_constructor <- smooth_spec_wrapper(spec_object, deriv = order, eps = eps)
  attr(expr, "basis") <- memoise::memoise(basis_constructor)
  attr(expr, "basis_label") <- deparse(s_call, width.cutoff = 60)[1]
  attr(expr, "basis_args") <- s_args
  attr(expr, "basis_matrix") <- basis_constructor(arg)
  attr(expr, "basis_deriv") <- order
  attr(expr, "domain") <- range(arg)
  expr
}

deriv_tfb_fpc <- function(expr, order = 1, lower, upper, 
  arg = tidyfun::arg(expr)) {
  efunctions <- environment(attr(expr, "basis"))$efunctions
  environment(attr(expr, "basis")) <- new.env()
  new_basis <- if (order > 0) {
    deriv(efunctions, order = order)
  } else {
    integrate(efunctions, lower = lower, upper = upper, definite = FALSE, 
      arg = arg)
  }  
  environment(attr(expr, "basis"))$efunctions <- new_basis
  attr(expr, "basis_matrix") <- t(as.matrix(new_basis))
  attr(expr, "arg") <- arg(new_basis)
  attr(expr, "domain") <- range(arg(new_basis))
  expr
}



#-------------------------------------------------------------------------------

#' Derivatives and integrals of functional data
#'
#' Derivatives of `tf`s use finite differences of the evaluations 
#' for `tfd` and finite differences of the basis functions for `tfb`. 
#' Note that, for some spline bases like `"cr"` or `"tp"` which always begin/end linearly,
#' computing second derivatives will produce artefacts at the outer limits 
#' of the functions' domain due to these boundary constraints. Basis `"bs"` does 
#' not have this problem, but tends to yield slightly less stable fits.
#'
#' @param expr a `tf`
#' @param order order of differentiation. Maximally 2 for `tfb` with `mgcv`-spline bases.
#' @param arg grid to use for the differentiation/integration. Not the `arg` of the returned object.
#' @param ... not used
#'
#' @return a `tf` with (slightly) different `arg` (and `basis`)
#' @export
#' @importFrom stats deriv
#' @rdname tfcalculus
deriv.tfd <- function(expr, order = 1, arg = NULL, ...) {
  #TODO: should this interpolate back to the original grid?
  # shortens the domain (slightly), for now.
  # this is necessary so that we don't get NAs when trying to evaluate derivs over 
  # their default domain etc. 
  if (is_irreg(expr)) warning("differentiating irregular data could be sketchy.")
  data <- as.matrix(expr, arg = arg, interpolate = TRUE)
  arg <- as.numeric(colnames(data))
  derived <- deriv_matrix(data, arg, order)
  ret <- tfd(derived$data, derived$arg, 
    domain = range(derived$arg), #!! shorter
    resolution = tidyfun:::resolution(expr))
  tf_evaluator(ret) <- attr(expr, "evaluator_name")
  ret
}
#' @export
#' @rdname tfcalculus
deriv.tfb <- function(expr, order = 1, ...) {
  deriv_tfb_mgcv(expr, order = order)
}
#' @export
#' @rdname tfcalculus
deriv.tfb_fpc <- function(expr, order = 1, ...) {
  deriv_tfb_fpc(expr, order = order)
}

#-------------------------------------------------------------------------------

#' @rdname tfcalculus
#' @description Integration of `tf`s is done by quadrature (trapezoid rule, specifically). 
#'  By default the scalar definite integral \eqn{\int^{upper}_{lower}f(s)ds} is returned 
#'  (option `definite = TRUE`), alternatively for `definite = FALSE` something 
#'  like the *anti-derivative* on `[lower, upper]`, e.g. an `tfd` object
#'  representing \eqn{F(t) = \int^{t}_{lower}f(s)ds}, for \eqn{t \in}`[lower, upper]`,
#'  is returned.
#' @details `integrate.function` is simply a wrapper for [stats::integrate()].
#' @export
integrate <- function(f, lower, upper, ...) {
  UseMethod("integrate")
}

integrate.default <- function(f, lower, upper, ...) .NotYetImplemented()
integrate.function <- stats::integrate

#' @rdname tfcalculus
#' @param f a `tf` object (or a `function`, see details)
#' @param lower lower limits of the integration range. For `definite=TRUE`, this can be
#'  a vector of the same length as `f`.
#' @param upper upper limits of the integration range (but see `definite` arg / Description).
#'  For `definite=TRUE`, this can be a vector of the same length as `f`.
#' @param definite should the definite integral  be returned (default)
#'   or the antiderivative. See Description.
#' @export
integrate.tfd <- function(f, lower = tf_domain(f)[1], upper = tf_domain(f)[2], 
  definite = TRUE, arg, ...) {
  if (missing(arg)) {
    arg <- tidyfun::arg(f)
  } else assert_arg(arg, f)
  arg <- ensure_list(arg)
  assert_numeric(lower, lower = tf_domain(f)[1], upper =  tf_domain(f)[2], 
    any.missing = FALSE)
  assert_numeric(upper,  lower = tf_domain(f)[1], upper =  tf_domain(f)[2], 
    any.missing = FALSE)
  stopifnot(length(lower) %in% c(1, length(f)), 
    length(upper) %in% c(1, length(f)))
  limits <- cbind(lower, upper)
  if (nrow(limits) > 1) {
    if (!definite) .NotYetImplemented() #needs vd-data
    limits <- limits %>% split(1:nrow(limits))
  }
  arg <- map2(arg, ensure_list(limits),
    ~ c(.y[1], .x[.x > .y[1] & .x < .y[2]], .y[2]))
  evaluations <- evaluate(f, arg)
  quads <- map2(arg, evaluations, ~ quad_trapez(arg = .x, evaluations = .y))
  if (definite) {
    ret <- map(quads, sum) %>% unlist
    names(ret) <- names(f)
    ret
  } else {
    tfd(data = map(quads, cumsum), arg = unlist(arg), domain = limits, 
      resolution = tidyfun:::resolution(f), evaluator = tf_approx_linear)
  }
  # this is too slow:
  # turn into functions, return definite integrals
  # (Why the hell does this not work without vectorize....?)
  #map(f, ~ possibly(stats::integrate, list(value = NA))(
  #  Vectorize(as.function(.x)), lower = lower, upper = upper, ...)) %>% 
  #map("value") 
}
#' @rdname tfcalculus
#' @export
integrate.tfb <- function(f, lower = tf_domain(f)[1], upper = tf_domain(f)[2], 
  definite = TRUE, arg, ...) {
  if (missing(arg)) {
    arg <- tidyfun::arg(f)
  } else assert_arg(arg, f)
  assert_numeric(lower, lower = tf_domain(f)[1], upper =  tf_domain(f)[2], 
    any.missing = FALSE)
  assert_numeric(upper,  lower = tf_domain(f)[1], upper =  tf_domain(f)[2], 
    any.missing = FALSE)
  stopifnot(length(lower) %in% c(1, length(f)), 
    length(upper) %in% c(1, length(f)))
  if (definite) {
    return(integrate(tfd(f, arg = arg), lower = lower, upper = upper, 
      arg = arg))
  }
  limits <- cbind(lower, upper)
  if (nrow(limits) > 1) .NotYetImplemented() #needs vd-data
  arg <- c(limits[1], arg[arg > limits[1] & arg < limits[2]], 
    limits[2])
  if (grepl("s\\(arg", attr(f, "basis_label"))) {
    return(deriv_tfb_mgcv(f, order = -1, arg = arg))
  }
  if (grepl("FPC", attr(f, "basis_label"))) {
    return(deriv_tfb_fpc(f, order = -1, arg = arg, lower = lower, 
      upper = upper))
  }
}
