#' Utility functions for `fvector`-objects
#' 
#' A bunch of methods & utilities that do what they say.
#' @param f an `fvector` object
#' @param x an `fvector` object
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

#' @rdname fvectormethods
#' @export
evaluator <- function(f) {
  stopifnot(inherits(f, "feval"))
  attr(f, "evaluator")
}

#' @rdname fvectormethods
#' @export
basis <- function(f) {
  stopifnot(inherits(f, "fbase"))
  attr(f, "basis")
}

#' @rdname fvectormethods
#' @param value for `evaluator<-`: name of a function that can be used to interpolate an `feval`. Needs to
#'   accept vector arguments `x`, `argvals`, `evaluations` and return
#'   evaluations of the function defined by `argvals`, `evaluations` at `x`
#'   for `argvals<-`: a list of grid points, for internal use only.
#' @export
`evaluator<-` <- function(x, value) {
  evaluator <- 
    if (is.character(value))  get(value, mode = "function") else value
  stopifnot(inherits(x, "feval"), is.function(evaluator))
  assert_set_equal(names(formals(evaluator)), 
    c("x", "argvals", "evaluations")) 
  attr(x, "evaluator_name") <- deparse(substitute(value))
  attr(x, "evaluator") <- memoise(evaluator)
  x
}
# this only used internally in feval_irreg conversion functions.
#' @rdname fvectormethods
`argvals<-` <- function(x, value) {
  stopifnot(inherits(x, "feval_irreg"))
  value <- map(value, ~signif(.x, attr(x, "signif_argvals")))
  assert_argvals(value, x)
  ret <- map2(evaluations(x), value, ~list(argvals = .y, data = .x))
  attributes(ret) <- attributes(x)
  ret
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

