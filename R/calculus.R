deriv_matrix <- function(data, argvals, order) {
  for (i in 1:order) {
    delta <- diff(argvals)
    data <- t(diff(t(data))/delta)
    argvals <- (argvals[-1] + head(argvals,-1))/2
  }
  list(data = data, argvals = argvals)
}

deriv_fbase_mgcv <- function(expr, order = 1, ...) {
  #TODO: make this work for iterated application deriv(deriv(fb)) 
  stopifnot(is.null(attr(expr, "basis_deriv")))
  argvals <- argvals(expr)
  s_args <- attr(expr, "basis_args")
  s_call <- as.call(c(quote(s), quote(argvals), s_args))
  s_spec <- eval(s_call)
  spec_object <- smooth.construct(s_spec, 
    data = data.frame(argvals = argvals), knots = NULL)
  eps <- min(diff(argvals)) / 1000
  basis_constructor <- smooth_spec_wrapper(spec_object, deriv = order, eps = eps)
  attr(expr, "basis") <- memoise::memoise(basis_constructor)
  attr(expr, "basis_label") <- deparse(s_call, width.cutoff = 60)[1]
  attr(expr, "basis_args") <- s_args
  attr(expr, "basis_matrix") <- basis_constructor(argvals)
  attr(expr, "basis_deriv") <- order
  expr
}

deriv_fbase_fpc <- function(expr, order = 1, ...) {
  efunctions <- environment(attr(expr, "basis"))$efunctions
  environment(attr(expr, "basis")) <- new.env()
  dfunctions <- deriv(efunctions, order = order)
  environment(attr(expr, "basis"))$efunctions <- dfunctions
  attr(expr, "basis_matrix") <- t(as.matrix(dfunctions))
  attr(expr, "argvals") <- argvals(dfunctions)
  expr
}

quad_trapez <- function(evaluations, argvals) {
<<<<<<< HEAD
  c(0, 0.5 * diff(argvals) * (evaluations[-1] + head(evaluations, -1)))
=======
  0.5 * diff(argvals) * (evaluations[-1] + head(evaluations, -1))
>>>>>>> a20562fd176dc2019420ad522aa2367f486f31fa
}

#-------------------------------------------------------------------------------

#' Derivatives and integrals of functional data
#'
#' Derivatives of `fvectors` use finite differences of the evaluations 
#' for `feval` and finite differences of the basis functions for `fbase`. 
#' Note that second derivatives for the latter will produce artefacts at the 
#' outer limits of the functions' domain due to boundary constraints for 
#' some spline bases like `"cr"` or `"tp"` which always end linearly. 
#'
#' @param expr an `fvector`
#' @param order order of differentiation. Maximally 2 for `fbase` with `mgcv`-spline bases.
#' @param argvals grid to use for the differentiation. Not the `argvals` of the returned object.
#' @param ... not used
#'
#' @return an `fvector` with (slightly) different `argvals` (and `basis`)
#' @export
#' @importFrom stats deriv
#' @rdname fvectorcalculus
deriv.feval <- function(expr, order = 1, argvals = NULL, ...) {
  #TODO: should this interpolate back to the original grid?
  if (is_irreg(expr)) warning("differentiating irregular data could be sketchy.")
  data <- as.matrix(expr, argvals = argvals, interpolate = TRUE)
  argvals <- as.numeric(colnames(data))
  derived <- deriv_matrix(data, argvals, order)
  ret <- feval(derived$data, derived$argvals, 
    domain = domain(expr), signif = attr(expr, "signif_argvals"))
  evaluator(ret) <- attr(expr, "evaluator_name")
  ret
}
#' @export
#' @rdname fvectorcalculus
deriv.fbase <- function(expr, order = 1, ...) {
  if (grepl("s\\(argvals", attr(expr, "basis_label"))) {
    return(deriv_fbase_mgcv(expr, order = order, ...))
  }
  if (grepl("FPC", attr(expr, "basis_label"))) {
    return(deriv_fbase_fpc(expr, order = order, ...))
  }
}

#-------------------------------------------------------------------------------

#' @rdname fvectorcalculus
#' @export
integrate <- function(f, lower, upper, ...) {
  UseMethod("integrate")
}

integrate.default <- function(f, lower, upper, ...) .NotYetImplemented()
integrate.function <- stats::integrate
integrate.fvector <- function(f, lower = domain(f)[1], upper = domain(f)[2], 
  definite = TRUE, argvals, ...) {
  if (missing(argvals)) {
    argvals <- ensure_list(argvals(f))
  } else assert_argvals(argvals, f)
  assert_numeric(lower, finite = TRUE, any.missing = FALSE)
  assert_numeric(upper, finite = TRUE, any.missing = FALSE)
  stopifnot(length(lower) %in% c(1, length(f)), 
    length(upper) %in% c(1, length(f)))
  limits <- cbind(lower, upper)
  if (nrow(limits) > 1 & !definite) .NotYetImplemented() #needs vd-data
  limits <- adjust_resolution(limits, f) %>% split(1:nrow(limits))
  argvals <- map2(argvals, limits,
    ~ c(.y[1], .x[.x > .y[1] & .x < .y[2]], .y[2]))
 evaluations <- evaluate(f, argvals)
  quads <- map2(argvals, evaluations, ~ quad_trapez(.x, .y))
  if (definite) {
    return(map(quads, sum) %>% unlist)
  } else {
    return(list(argvals, quads))
  }
  # this is too slow:
  # turn into functions, return definite integrals
  #map(f, ~ possibly(stats::integrate, list(value = NA))(
  #  Vectorize(as.function(.x)), lower = lower, upper = upper, ....)) %>% 
  #map("value") 
}
