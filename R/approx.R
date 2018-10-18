#' @import zoo
zoo_wrapper <- function(f, ...) {
  dots <- list(...)
  function(x, arg, evaluations) {
    x_arg <- sort(unique(c(x, arg)))
    x_arg_match <- match(x_arg, arg, nomatch = length(arg) + 1)
    requested <- x_arg %in% x
    dots[[length(dots) + 1]] <- zoo(evaluations[x_arg_match], x_arg)
    ret <- do.call(f, dots)
    coredata(ret)[requested]
  }
}

#-------------------------------------------------------------------------------

#' @title Inter- and extrapolation functions for `tfd`-objects
#'
#' @description
#' These are the currently available `evaluator`-functions for `tfd`-objects,
#' which control how the entries are inter-/extrapolated to previously unseen
#' `arg`-values. They all are merely wrappers around [zoo::na.fill()],
#' [zoo::na.approx()], etc... Note that these are not meant to be called directly --
#' they are internal functions used by [tf_evaluate.tfd()] to do its thing.
#'
#' The list:
#'
#' - `tf_approx_linear` for linear interpolation without extrapolation (i.e.,
#' [zoo::na.approx()] with `na.rm = FALSE`)  -- this is the default,
#' - `tf_approx_spline` for cubic spline interpolation, (i.e., [zoo::na.spline()]
#' with `na.rm = FALSE`),
#' - `tf_approx_none` in order to not inter-/extrapolate ever (i.e., [zoo::na.fill()] with `fill = NA`)
#' - `tf_approx_fill_extend` for linear interpolation and constant extrapolation
#' (i.e., [zoo::na.fill()] with `fill = "extend"`)
#' - `tf_approx_locf` for "last observation carried forward"  (i.e.,
#' [zoo::na.locf()] with `na.rm = FALSE` and
#' - `tf_approx_nocb` for "next observation carried backward" (i.e.,
#' [zoo::na.locf()] with `na.rm = FALSE, fromLast = TRUE`).
#'
#' For implementing your own, see source code of `tidyfun:::zoo_wrapper`.
#'
#' @rdname tf_approx
#' @seealso tfd
#' @export
#' @param x new arg values to tf_approximate the function for
#' @param arg the `arg` values of the `evaluations`
#' @param evaluations the function's values at `arg`
tf_approx_linear <- zoo_wrapper(na.approx, na.rm = FALSE)

#' @rdname tf_approx
#' @export
tf_approx_spline <- zoo_wrapper(na.spline, na.rm = FALSE)

#' @rdname tf_approx
#' @export
tf_approx_none <- zoo_wrapper(na.fill, fill = NA)

#' @rdname tf_approx
#' @export
tf_approx_fill_extend <- zoo_wrapper(na.fill, fill = "extend")

#' @rdname tf_approx
#' @export
tf_approx_locf <- zoo_wrapper(na.locf, na.rm = FALSE)

#' @rdname tf_approx
#' @export
tf_approx_nocb <- zoo_wrapper(na.locf, na.rm = FALSE, fromLast = TRUE)
