#' Utility functions for `tf`-objects
#' 
#' A bunch of methods & utilities that do what they say: extract or set the
#' respective attributes of a `tf`-object.
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
tf_evaluations <- function(f) UseMethod("tf_evaluations")
#' @export
tf_evaluations.default <- function(f) .NotYetImplemented()
#' @export
tf_evaluations.tfd_reg <- function(f) {
  attributes(f) <- NULL
  f
}
#' @export
tf_evaluations.tfd_irreg <- function(f) {
  map(f, "value")
}
#' @export
tf_evaluations.tfb <- function(f) {
  map(f, ~ drop(attr(f, "basis_matrix") %*% .))
} 


#' @rdname tfmethods
#' @export
tf_count <- function(f) UseMethod("tf_count")
#' @export
tf_count.default <- function(f) .NotYetImplemented()
#' @export
tf_count.tfd_irreg <- function(f) {
  ret <- map_int(tf_evaluations(f), length)
  ret[is.na(f)] <- 0
  ret
}  
#' @export
tf_count.tfd_reg <- function(f) length(arg(f))

#' @rdname tfmethods
#' @export
tf_domain <- function(f) {
  stopifnot(inherits(f, "tf"))
  attr(f, "domain")
}

#' @rdname tfmethods
#' @param forget extract the evaluator or basis-creating function without its cache? 
#'   See [memoise::forget()]. Defaults to `FALSE`.
#' @export
tf_evaluator <- function(f, forget = FALSE) {
  stopifnot(inherits(f, "tfd"))
  ret <- attr(f, "evaluator")
  if (forget) forget(ret)
  ret
}

#' @rdname tfmethods
#' @param as_tfd should the basis be returned as a `tfd` evaluated on `arg(f)`? Defaults to FALSE.
#' @export
tf_basis <- function(f, as_tfd = FALSE, forget = FALSE) {
  stopifnot(inherits(f, "tfb"))
  basis <- attr(f, "basis")
  if (forget) forget(basis)
  if (!as_tfd) return(basis)
  basis(arg(f)) %>% t %>% tfd(arg = arg(f))
}

#' @rdname tfmethods
#' @param value **for `evaluator<-`:** (bare or quoted) name of a function that
#'   can be used to interpolate an `tfd`. Needs to accept vector arguments `x`,
#'   `arg`, `evaluations` and return evaluations of the function defined by
#'   `arg`, `evaluations` at `x`
#'   **for `arg<-`:** a (list of) new `arg`-values
#' @export
`tf_evaluator<-` <- function(x, value) {
  value <- if (is.function(value)) {
    deparse(substitute(value))
  } else quo_name(enexpr(value))
  stopifnot(is_tfd(x))
  evaluator <- get(value, mode = "function", envir = parent.frame())
  stopifnot(inherits(x, "tfd"))
  assert_set_equal(names(formals(evaluator)), 
    c("x", "arg", "evaluations")) 
  attr(x, "evaluator_name") <- value
  attr(x, "evaluator") <- memoise(evaluator)
  x
}

#' @rdname tfmethods
#' @export
`arg<-` <- function(x, value) UseMethod("arg<-")

#' @rdname tfmethods
#' @export
`arg<-.tfd_irreg` <- function(x, value) {  
  assert_arg(value, x)
  ret <- map2(tf_evaluations(x), value, ~list(arg = .y, data = .x))
  attributes(ret) <- attributes(x)
  forget(attr(ret, "evaluator"))
  ret
}

#' @rdname tfmethods
#' @export
`arg<-.tfd_reg` <- function(x, value) {
  assert_arg(value, x)
  attr(x, "arg") <- value
  forget(attr(x, "evaluator"))
  x
}

#' @rdname tfmethods
#' @export
tf_resolution <- function(f) {
  attr(f, "resolution")
}

#TODO: add pipe-able modify_xx that call assignment functions on their first arg

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

#-------------------------------------------------------------------------------

#' @rdname tfmethods
#' @export
is.na.tf <- function(x) {
  map_lgl(unclass(x), ~ is.na(.x)[1])
}
#' @rdname tfmethods
#' @export
is.na.tfd_irreg <- function(x) {
  map_lgl(unclass(x), ~ is.na(.x$value[1]))
}


#-------------------------------------------------------------------------------

#' @rdname tfmethods
#' @export
is_tf <- function(x) "tf" %in% class(x)

#' @rdname tfmethods
#' @export
is_irreg <- function(x) "tfd_irreg" %in% class(x)

#' @rdname tfmethods
#' @export
is_tfd <- function(x) "tfd" %in% class(x)

#' @rdname tfmethods
#' @export
is_tfb <- function(x) "tfb" %in% class(x)

