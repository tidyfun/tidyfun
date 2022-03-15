#' Re-evaluate `tf`-objects on new evaluation grid.
#'
#' @description Change the internal representation of a `tf`-object so that it
#' uses a different grid of evaluation points (`arg`). Useful for
#'
#' - thinning out dense grids to make data smaller 
#' - filling out sparse grids to make derivatives/integrals and locating extrema or zero crossings more
#' accurate (... *if* the interpolation works well ...) 
#' - making irregular functional data into (more) regular data. 
#'
#' This is really just syntactic sugar for `tf<d|b>(object, arg = arg)`.
#' To reliably impute very irregular data on a regular, common grid, 
#' you'll be better off doing FPCA-based imputation or other model-based
#' approaches in most cases.
#'
#' @param object an object inheriting from `tf`
#' @param arg a vector of argument values on which to evaluate the functions in
#'   `object`
#' @param ...  additional arguments handed over to `tfd` or `tfb`, for the
#'   construction of the returned object
#' @return a `tfd` or `tfb` object on the new grid given by `arg`
#'
#' @export
#' @examples
#' # thinning out a densely observed tfd
#' (dense <- tf_rgp(10, arg = seq(0, 1, l = 1001)))
#' (less_dense <- tf_interpolate(dense, arg = seq(0, 1, l = 101)))
#'
#' # filling out sparse data (use a suitable evaluator -function!)
#' sparse <- tf_rgp(10, arg = seq(0, 5, l = 21))
#' plot(sparse)
#' tfd(sparse, evaluator= tf_approx_spline) |>   #change eval. for better interpolation
#'   tf_interpolate(arg = seq(0, 5, l = 201)) |>
#'   lines(col = 2)
#'
#' set.seed(1860)
#' (sparse_irregular <- tf_rgp(5) |>  tf_sparsify(.5) |> tf_jiggle())
#' tf_interpolate(sparse_irregular, arg = seq(0, 1, l = 51))
#' 
tf_interpolate <- function(object, arg, ...) UseMethod("tf_interpolate")

#' @export
#' @rdname tf_interpolate
tf_interpolate.tfb <- function(object, arg, ...) {
  stopifnot(!missing(arg))
  tfb(object, arg = arg, ...)
}

#' @export
#' @rdname tf_interpolate
tf_interpolate.tfd <- function(object, arg, ...) {
  stopifnot(!missing(arg))
  tfd(object, arg = arg, ...)
}

# #' @export
# #' @rdname tf_interpolate
# tf_interpolate.tfd_irreg <- function(object, arg, force = FALSE, ...) {
#   stopifnot(!missing(arg))
#   ret <- suppressWarnings(tfd(object, arg = arg, ...))
#   if (force & is_irreg(ret) & !is.list(arg)) {
#     # fill up NAs through LOCF/NOCB
#     ret <- suppressWarnings(tfd(ret, arg = arg, evaluator = tf_approx_fill_extend))
#     tf_evaluator(ret) <- list(...)$evaluator %||% attributes(object)$evaluator_name
#   }
#   if (is_irreg(ret)) {
#     warning("tf_interpolate did not create regular data on the supplied <arg>-values.")
#   }
#   ret
# }
