#' Utility functions for `tf`-objects
#' 
#' A bunch of methods & utilities that do what they say.
#' @param f an `tf` object
#' @param x an `tf` object
#' @rdname tfmethods
#' @export
arg <- function(f) UseMethod("arg")
#' @export
arg.default <- function(f) .NotYetImplemented()
#' @export
arg.tfd_irreg <- function(f) map(f, "arg")
#' @export
arg.tfd_reg <- function(f) attr(f, "arg")[[1]]
#' @export
arg.tfb <- function(f) attr(f, "arg")

#' @rdname tfmethods
#' @export
evaluations <- function(f) UseMethod("evaluations")
#' @export
evaluations.default <- function(f) .NotYetImplemented()
#' @export
evaluations.tfd_reg <- function(f) {
  attributes(f) <- NULL
  f
}
#' @export
evaluations.tfd_irreg <- function(f) {
  map(f, "value")
}
#' @export
evaluations.tfb <- function(f) {
  map(f, ~ drop(attr(f, "basis_matrix") %*% .))
} 


#' @rdname tfmethods
#' @export
n_evaluations <- function(f) UseMethod("n_evaluations")
#' @export
n_evaluations.default <- function(f) .NotYetImplemented()
#' @export
n_evaluations.tfd_irreg <- function(f) map_int(evaluations(f), length)
#' @export
n_evaluations.tfd_reg <- function(f) length(arg(f))

#' @rdname tfmethods
#' @export
domain <- function(f) {
  stopifnot(inherits(f, "tf"))
  attr(f, "domain")
}

#' @rdname tfmethods
#' @export
evaluator <- function(f) {
  stopifnot(inherits(f, "tfd"))
  attr(f, "evaluator")
}

#' @rdname tfmethods
#' @export
basis <- function(f) {
  stopifnot(inherits(f, "tfb"))
  attr(f, "basis")
}

#' @rdname tfmethods
#' @param value for `evaluator<-`: name of a function that can be used to interpolate an `tfd`. Needs to
#'   accept vector arguments `x`, `arg`, `evaluations` and return
#'   evaluations of the function defined by `arg`, `evaluations` at `x`
#'   for `arg<-`: a list of grid points, for internal use only.
#' @export
`evaluator<-` <- function(x, value) {
  evaluator <- 
    if (is.character(value))  get(value, mode = "function") else value
  stopifnot(inherits(x, "tfd"), is.function(evaluator))
  assert_set_equal(names(formals(evaluator)), 
    c("x", "arg", "evaluations")) 
  attr(x, "evaluator_name") <- deparse(substitute(value))
  attr(x, "evaluator") <- memoise(evaluator)
  x
}
# this only used internally in tfd_irreg conversion functions.
#' @rdname tfmethods
`arg<-` <- function(x, value) {
  stopifnot(inherits(x, "tfd_irreg"))
  value <- map(value, ~signif(.x, attr(x, "signif_arg")))
  assert_arg(value, x)
  ret <- map2(evaluations(x), value, ~list(arg = .y, data = .x))
  attributes(ret) <- attributes(x)
  ret
}

#-------------------------------------------------------------------------------

#' @rdname tfmethods
#' @param object as usual
#' @param ... dots
#' @export
#' @importFrom stats coef
coef.tfb <- function(object, ...) {
  attributes(object) <- NULL
  object
}

#' @export
#' @rdname tfmethods
rev.tf <- function(x) {
  x[rev(seq_along(x))]
}

