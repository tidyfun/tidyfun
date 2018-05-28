# compute derivatives of data rows by finite differences. 
# returns derivaitves at interval midpoints
deriv_matrix <- function(data, argvals, order) {
  for (i in 1:order) {
    delta <- diff(argvals)
    data <- t(diff(t(data))/delta)
    argvals <- (argvals[-1] + head(argvals,-1))/2
  }
  list(data = data, argvals = argvals)
}

#trapezoidal quadrature
quad_trapez <- function(argvals, evaluations) {
  c(0, 0.5 * diff(argvals) * (head(evaluations, -1) + evaluations[-1]))
}

deriv_fbase_mgcv <- function(expr, order = 1, argvals = tidyfun::argvals(expr)) {
  #TODO: make this work for iterated application deriv(deriv(fb)) 
  if (!is.null(attr(expr, "basis_deriv"))) 
    stop("Can't derive or integrate previously derived/integrated fvector in basis representation")
  s_args <- attr(expr, "basis_args")
  s_call <- as.call(c(quote(s), quote(argvals), s_args))
  s_spec <- eval(s_call)
  spec_object <- smooth.construct(s_spec, 
    data = data.frame(argvals =  argvals), knots = NULL)
  eps <- min(diff(argvals)) / 1000
  basis_constructor <- smooth_spec_wrapper(spec_object, deriv = order, eps = eps)
  attr(expr, "basis") <- memoise::memoise(basis_constructor)
  attr(expr, "basis_label") <- deparse(s_call, width.cutoff = 60)[1]
  attr(expr, "basis_args") <- s_args
  attr(expr, "basis_matrix") <- basis_constructor(argvals)
  attr(expr, "basis_deriv") <- order
  attr(expr, "domain") <- range(argvals)
  expr
}

deriv_fbase_fpc <- function(expr, order = 1, lower, upper, 
  argvals = tidyfun::argvals(expr)) {
  efunctions <- environment(attr(expr, "basis"))$efunctions
  environment(attr(expr, "basis")) <- new.env()
  new_basis <- if (order > 0) {
    deriv(efunctions, order = order)
  } else {
    integrate(efunctions, lower = lower, upper = upper, definite = FALSE, 
      argvals = argvals)
  }  
  environment(attr(expr, "basis"))$efunctions <- new_basis
  attr(expr, "basis_matrix") <- t(as.matrix(new_basis))
  attr(expr, "argvals") <- argvals(new_basis)
  attr(expr, "domain") <- range(argvals(new_basis))
  expr
}



#-------------------------------------------------------------------------------

#' Derivatives and integrals of functional data
#'
#' Derivatives of `fvectors` use finite differences of the evaluations 
#' for `feval` and finite differences of the basis functions for `fbase`. 
#' Note that, for some spline bases like `"cr"` or `"tp"` which always begin/end linearly,
#' computing second derivatives will produce artefacts at the outer limits 
#' of the functions' domain due to these boundary constraints. Basis `"bs"` does 
#' not have this problem, but tends to yield slightly less stable fits.
#'
#' @param expr an `fvector`
#' @param order order of differentiation. Maximally 2 for `fbase` with `mgcv`-spline bases.
#' @param argvals grid to use for the differentiation/integration. Not the `argvals` of the returned object.
#' @param ... not used
#'
#' @return an `fvector` with (slightly) different `argvals` (and `basis`)
#' @export
#' @importFrom stats deriv
#' @rdname fvectorcalculus
deriv.feval <- function(expr, order = 1, argvals = NULL, ...) {
  #TODO: should this interpolate back to the original grid?
  # shortens the domain (slightly), for now.
  if (is_irreg(expr)) warning("differentiating irregular data could be sketchy.")
  data <- as.matrix(expr, argvals = argvals, interpolate = TRUE)
  argvals <- as.numeric(colnames(data))
  derived <- deriv_matrix(data, argvals, order)
  ret <- feval(derived$data, derived$argvals, 
    domain = range(derived$argvals), #!! shorter
    signif = attr(expr, "signif_argvals"))
  evaluator(ret) <- attr(expr, "evaluator_name")
  ret
}
#' @export
#' @rdname fvectorcalculus
deriv.fbase <- function(expr, order = 1, ...) {
  if (grepl("s\\(argvals", attr(expr, "basis_label"))) {
    return(deriv_fbase_mgcv(expr, order = order))
  }
  if (grepl("FPC", attr(expr, "basis_label"))) {
    return(deriv_fbase_fpc(expr, order = order))
  }
}

#-------------------------------------------------------------------------------

#' @rdname fvectorcalculus
#' @description Integration of `fvectors` is done by quadrature (trapezoid rule, specifically). 
#'  By default the scalar definite integral \eqn{\int^{upper}_{lower}f(s)ds} is returned 
#'  (option `definite = TRUE`), alternatively for `definite = FALSE` something 
#'  like the *anti-derivative* on `[lower, upper]`, e.g. an `feval` object
#'  representing \eqn{F(t) = \int^{t}_{lower}f(s)ds}, for \eqn{t \in}`[lower, upper]`,
#'  is returned.
#' @export
integrate <- function(f, lower, upper, ...) {
  UseMethod("integrate")
}

integrate.default <- function(f, lower, upper, ...) .NotYetImplemented()

#' @rdname fvectorcalculus
#' @details `integrate.function` is simply a wrapper for [stats::integrate()].
#' @export
integrate.function <- stats::integrate

#' @rdname fvectorcalculus
#' @param lower lower limits of the integration range. For `definite=TRUE`, this can be
#'  a vector of the same length as `f`.
#' @param upper upper limits of the integration range (but see `definite` arg / Description).
#'  For `definite=TRUE`, this can be a vector of the same length as `f`.
#' @param definite should the definite integral  be returned (default)
#'   or the antiderivative. See Description.
#' @export
integrate.feval <- function(f, lower = domain(f)[1], upper = domain(f)[2], 
  definite = TRUE, argvals, ...) {
  if (missing(argvals)) {
    argvals <- tidyfun::argvals(f)
  } else assert_argvals(argvals, f)
  argvals <- ensure_list(argvals)
  assert_numeric(lower, lower = domain(f)[1], upper =  domain(f)[2], 
    any.missing = FALSE)
  assert_numeric(upper,  lower = domain(f)[1], upper =  domain(f)[2], 
    any.missing = FALSE)
  stopifnot(length(lower) %in% c(1, length(f)), 
    length(upper) %in% c(1, length(f)))
  limits <- cbind(lower, upper)
  if (nrow(limits) > 1) {
    if (!definite) .NotYetImplemented() #needs vd-data
    limits <- adjust_resolution(limits, f) %>% split(1:nrow(limits))
  }
  argvals <- map2(argvals, ensure_list(limits),
    ~ c(.y[1], .x[.x > .y[1] & .x < .y[2]], .y[2]))
  evaluations <- evaluate(f, argvals)
  quads <- map2(argvals, evaluations, ~ quad_trapez(argvals = .x, evaluations = .y))
  if (definite) {
    return(map(quads, sum) %>% unlist)
  } else {
    feval(data = map(quads, cumsum), argvals = unlist(argvals), domain = limits, 
      signif = attr(f, "signif_argvals"), evaluator = approx_linear)
  }
  # this is too slow:
  # turn into functions, return definite integrals
  # (Why the hell does this not work without vectorize....?)
  #map(f, ~ possibly(stats::integrate, list(value = NA))(
  #  Vectorize(as.function(.x)), lower = lower, upper = upper, ...)) %>% 
  #map("value") 
}
#' @rdname fvectorcalculus
#' @export
integrate.fbase <- function(f, lower = domain(f)[1], upper = domain(f)[2], 
  definite = TRUE, argvals, ...) {
  if (missing(argvals)) {
    argvals <- tidyfun::argvals(f)
  } else assert_argvals(argvals, f)
  assert_numeric(lower, lower = domain(f)[1], upper =  domain(f)[2], 
    any.missing = FALSE)
  assert_numeric(upper,  lower = domain(f)[1], upper =  domain(f)[2], 
    any.missing = FALSE)
  stopifnot(length(lower) %in% c(1, length(f)), 
    length(upper) %in% c(1, length(f)))
  if (definite) {
    return(integrate(feval(f, argvals = argvals), lower = lower, upper = upper, 
      argvals = argvals))
  }
  limits <- cbind(lower, upper)
  if (nrow(limits) > 1) .NotYetImplemented() #needs vd-data
  limits <- adjust_resolution(limits, f)
  argvals <- c(limits[1], argvals[argvals > limits[1] & argvals < limits[2]], 
    limits[2])
  if (grepl("s\\(argvals", attr(f, "basis_label"))) {
    return(deriv_fbase_mgcv(f, order = -1, argvals = argvals))
  }
  if (grepl("FPC", attr(f, "basis_label"))) {
    return(deriv_fbase_fpc(f, order = -1, argvals = argvals, lower = lower, 
      upper = upper))
  }
}
