#' Utility functions for `fvector`-objects
#' 
#' A bunch of methods that do what they say.
#' @param f an `fvector` object
#' 
#' @rdname fvectormethods
#' @export
argvals <- function(f) UseMethod("argvals")
#' @export
argvals.default <- function(f) .NotYetImplemented()
#' @export
argvals.feval_irreg <- function(f) map(f, "argvals")
#' @export
argvals.feval_reg <- function(f) attr(f, "argvals")[[1]]
#' @export
argvals.fbase <- function(f) attr(f, "argvals")

#' @rdname fvectormethods
#' @export
evaluations <- function(f) UseMethod("evaluations")
#' @export
evaluations.default <- function(f) .NotYetImplemented()
#' @export
evaluations.feval_reg <- function(f) {
  attributes(f) <- NULL
  f
}
#' @export
evaluations.feval_irreg <- function(f) {
  map(f, "data")
}
#' @export
evaluations.fbase <- function(f) {
  map(f, ~ drop(attr(f, "basis_matrix") %*% .))
} 


#' @rdname fvectormethods
#' @export
n_evaluations <- function(f) UseMethod("n_evaluations")
#' @export
n_evaluations.default <- function(f) .NotYetImplemented()
#' @export
n_evaluations.feval_irreg <- function(f) map_int(evaluations(f), length)
#' @export
n_evaluations.feval_reg <- function(f) length(argvals(f))

#' @rdname fvectormethods
#' @export
domain <- function(f) {
  stopifnot(inherits(f, "fvector"))
  attr(f, "domain")
}
  
#' @export
evaluator <- function(f) {
  stopifnot(inherits(f, "feval"))
  attr(f, "evaluator")
}

#' @export
basis <- function(f) {
  stopifnot(inherits(f, "fbase"))
  attr(f, "basis")
}

#' @rdname fvectormethods
#' @param value for `evaluator<-`: a function that can be used to interpolate an `feval`. Needs to
#'   accept vector arguments `x`, `argvals`, `evaluations` and return
#'   evaluations of the function defined by `argvals`, `evaluations` at `x`
#'   for `argvals<-`: a list of grid points, for internal use only.
#' @export
`evaluator<-` <- function(x, value) {
  stopifnot(inherits(x, "feval"), is.function(value))
  attr(x, "evaluator_name") <- deparse(value, width.cutoff = 60)[1]
  attr(x, "evaluator") <- memoise(eval(value))
  x
}
# this only used internally in feval_irreg conversion functions.
#' @rdname fvectormethods
`argvals<-` <- function(x, value) {
  stopifnot(inherits(x, "feval_irreg"))
  value <- map(value, ~signif(.x, attr(x, "signif_argvals")))
  assert_argvals(value, x)
  attr_ret <- attributes(x)
  ret <- map2(evaluations(x), value, ~list(argvals = .y, data = .x))
  attributes(ret) <- attr_ret
  ret
}

#-------------------------------------------------------------------------------

is.na.fvector <- function(x) {
  map_lgl(unclass(x), ~ any(is.na(.x)))
}

#-------------------------------------------------------------------------------

#' @rdname fvectormethods
#' @param object as usual
#' @param ... dots
#' @export
#' @importFrom stats coef
coef.fbase <- function(object, ...) {
  attributes(object) <- NULL
  object
}

#' @export
#' @rdname fvectormethods
rev.fvector <- function(x) {
  x[rev(seq_along(x))]
}

#' @export
#' @rdname fvectormethods
mean.fvector <- function(x, ...){
  summarize_fvector(x, op = "mean", eval  = is_feval(x), ...)
}

#' @param depth method used to determine the most central element in `x`, i.e., the median.
#'  One of the functional data depths available via [depth()] or `"pointwise"` for
#'  a pointwise median function. 
#' @importFrom stats median
#' @export
#' @rdname fvectormethods
median.fvector <- function(x, na.rm = FALSE, depth = c("MBD", "pointwise"), ...){
  if (!na.rm) {
    if (any(is.na(x))) return(1 * NA * x[1])
  } else {
    x <- x[!is.na(x)]
  }
  depth  <- match.arg(depth)
  if (depth == "pointwise") {
    summarize_fvector(x, na.rm = na.rm, op = "median", eval  = is_feval(x), ...)
  } else {
    depths <- depth(x, depth = depth)
    x[depths == max(depths)]
  }
}

#' @importFrom stats quantile
#' @inheritParams stats::quantile
#' @export
#' @rdname fvectormethods
quantile.fvector <- function(x, probs = seq(0, 1, 0.25), na.rm = FALSE,
  names = TRUE, type = 7, ...){
  #TODO: functional quantiles will need (a lot) more thought, 
  # cf. Serfling, R., & Wijesuriya, U. (2017). 
  # Depth-based nonparametric description of functional data, with emphasis on use of spatial depth.
  warning("only pointwise, non-functional quantiles implemented for fvectors.")
  summarize_fvector(x, probs = probs, na.rm = na.rm,
    names = names, type = type, op = "quantile", eval  = is_feval(x), ...)
}

#' @inheritParams stats::sd
#' @export
#' @rdname fvectormethods
sd <- function(x, na.rm = FALSE) UseMethod("sd")

#' @importFrom stats sd
#' @rdname fvectormethods
sd.default <- stats::sd

#' @export
#' @rdname fvectormethods
sd.fvector <- function(x, na.rm = FALSE){
  summarize_fvector(x, na.rm = na.rm, op = "sd", eval  = is_feval(x))
} 

#' @inheritParams stats::var
#' @export
#' @rdname fvectormethods
var <- function(x, y = NULL, na.rm = FALSE, use) UseMethod("var")

#' @export
#' @importFrom stats sd
#' @rdname fvectormethods
var.default <- stats::var

#' @export
#' @rdname fvectormethods
var.fvector <- function(x, y = NULL, na.rm = FALSE, use){
  summarize_fvector(x, na.rm = na.rm, op = "sd", eval  = is_feval(x))
} 

# deriv
# cov / cor # needs image class/fpca methods

#summary 
#' @export
summary.fvector <- function(object, ...) {
  depths <- depth(object, ...)
  central <- which(depths <= median(depths))
  c(mean = mean(object), var = var(object),
    median = object[which.max(depths)], 
    central_half = range(object[central]))
}

### new generics:
# integrate

