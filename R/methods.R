#' Utility functions for `tf`-objects
#'
#' A bunch of methods & utilities that do what they say: extract or set the
#' respective attributes of a `tf`-object.
#' @param f an `tf` object
#' @param x an `tf` object
#' @rdname tfmethods
#' @export
tf_arg <- function(f) UseMethod("tf_arg")
#' @export
tf_arg.default <- function(f) .NotYetImplemented()
#' @export
tf_arg.tfd_irreg <- function(f) map(f, "arg")
#' @export
tf_arg.tfd_reg <- function(f) attr(f, "arg")[[1]]
#' @export
tf_arg.tfb <- function(f) attr(f, "arg")

#' @rdname tfmethods
#' @export
tf_evaluations <- function(f) UseMethod("tf_evaluations")
#' @export
tf_evaluations.default <- function(f) .NotYetImplemented()
#' @export
tf_evaluations.tfd_reg <- function(f) {
  f_names <- names(f)
  attributes(f) <- NULL
  names(f) <- f_names
  f
}
#' @export
tf_evaluations.tfd_irreg <- function(f) {
  map(f, "value")
}
#' @export
tf_evaluations.tfb <- function(f) {
  evals <- map(f, ~ drop(attr(f, "basis_matrix") %*% .))
  if (!inherits(f, "tfb_fpc")) {
    evals <- map(evals, attr(f, "family")$linkinv)
  }
  evals
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
tf_count.tfd_reg <- function(f) length(tf_arg(f))

#' @rdname tfmethods
#' @export
tf_domain <- function(f) {
  stopifnot(inherits(f, "tf"))
  attr(f, "domain")
}
#' @rdname tfmethods
#' @export
`tf_domain<-` <- function(x, value) {
  stopifnot(inherits(x, "tf"))
  assert_numeric(value, any.missing = FALSE, len = 2, unique = TRUE, sorted = TRUE)
  tf_zoom(f = x, begin = value[1], end = value[2])
}


#' @rdname tfmethods
#' @export
tf_evaluator <- function(f) {
  stopifnot(inherits(f, "tfd"))
  attr(f, "evaluator")
}

#' @rdname tfmethods
#' @param as_tfd should the basis be returned as a `tfd` evaluated on `tf_arg(f)`? Defaults to FALSE.
#' @export
tf_basis <- function(f, as_tfd = FALSE) {
  stopifnot(inherits(f, "tfb"))
  basis <- attr(f, "basis")
  if (!as_tfd) return(basis)
  basis(tf_arg(f)) %>% t() %>% tfd(arg = tf_arg(f))
}

#' @rdname tfmethods
#' @param value **for `tf_evaluator<-`:** (bare or quoted) name of a function that
#'   can be used to interpolate an `tfd`. Needs to accept vector arguments `x`,
#'   `arg`, `evaluations` and return evaluations of the function defined by
#'   `arg`, `evaluations` at `x`. \cr  
#'   **for `tf_arg<-`:** (list of) new `arg`-values. \cr  
#'   **for `td_domain<-`:** sorted numeric vector with the 2 new endpoints of 
#'     the domain.
#' @export
`tf_evaluator<-` <- function(x, value) {
  value <- if (is.function(value)) {
    deparse(substitute(value))
  } else {
    quo_name(enexpr(value))
  }
  stopifnot(is_tfd(x))
  evaluator <- get(value, mode = "function", envir = parent.frame())
  stopifnot(inherits(x, "tfd"))
  assert_set_equal(
    names(formals(evaluator)),
    c("x", "arg", "evaluations")
  )
  attr(x, "evaluator_name") <- value
  attr(x, "evaluator") <- evaluator
  x
}

#' @rdname tfmethods
#' @export
`tf_arg<-` <- function(x, value) UseMethod("tf_arg<-")

#' @rdname tfmethods
#' @export
`tf_arg<-.tfd_irreg` <- function(x, value) {
  assert_arg(value, x)
  ret <- map2(tf_evaluations(x), value, ~list(arg = .y, data = .x))
  attributes(ret) <- attributes(x)
  ret
}

#' @rdname tfmethods
#' @export
`tf_arg<-.tfd_reg` <- function(x, value) {
  assert_arg(value, x)
  attr(x, "arg") <- ensure_list(value)
  x
}

#' @rdname tfmethods
#' @export
tf_resolution <- function(f) {
  attr(f, "resolution")
}

# TODO: add pipe-able modify_xx that call assignment functions on their first arg

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
  map_lgl(unclass(x), ~is.na(.x)[1])
}
#' @rdname tfmethods
#' @export
is.na.tfd_irreg <- function(x) {
  map_lgl(unclass(x), ~is.na(.x$value[1]))
}


#-------------------------------------------------------------------------------

#' @rdname tfmethods
#' @export
is_tf <- function(x) inherits(x, "tf")

#' @rdname tfmethods
#' @export
is_tfd <- function(x)  inherits(x, "tfd")
#' @rdname tfmethods
#' @export
is_reg <- function(x)  inherits(x, "tfd_reg")
#' @rdname tfmethods
#' @export
is_tfd_reg <- is_reg

#' @rdname tfmethods
#' @export
is_irreg <- function(x) inherits(x,"tfd_irreg")
#' @rdname tfmethods
#' @export
is_tfd_irreg <- is_irreg


#' @rdname tfmethods
#' @export
is_tfb <- function(x) inherits(x, "tfb")

#' @rdname tfmethods
#' @export
is_tfb_spline <- function(x) inherits(x, "tfb_spline")

#' @rdname tfmethods
#' @export
is_tfb_fpc <- function(x) inherits(x, "tfb_fpc")
