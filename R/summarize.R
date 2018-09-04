#' Functions that summarize `tf` objects
#'
#' These will return a `tf` object containing the respective functional statistic.
#' `summary` returns a vector with the mean function, the variance function, and the
#' functional range of the central half of the functions, as defined by the functional
#'
#' @param x a `tf` object
#'   functions, see source code.
#' @name tfsummaries
NULL

#' @export
#' @rdname tfsummaries
mean.tf <- function(x, ...) {
  summarize_tf(x, op = "mean", eval = is_tfd(x), ...)
}

#' @param depth method used to determine the most central element in `x`, i.e., the median.
#'  One of the functional data depths available via [depth()] or `"pointwise"` for
#'  a pointwise median function.
#' @importFrom stats median
#' @export
#' @rdname tfsummaries
median.tf <- function(x, na.rm = FALSE, depth = c("MBD", "pointwise"), ...) {
  if (!na.rm) {
    if (any(is.na(x))) return(1 * NA * x[1])
  } else {
    x <- x[!is.na(x)]
  }
  depth <- match.arg(depth)
  if (depth == "pointwise") {
    summarize_tf(x, na.rm = na.rm, op = "median", eval = is_tfd(x), ...)
  } else {
    tf_depths <- tf_depth(x, depth = depth)
    med <- x[tf_depths == max(tf_depths)]
    if (length(med) > 1) {
      warning(
        length(med),
        " observations with maximal depth, returning their mean."
      )
      mean(med)
    } else {
      med
    }
  }
}

#' @importFrom stats quantile
#' @inheritParams stats::quantile
#' @export
#' @rdname tfsummaries
quantile.tf <- function(x, probs = seq(0, 1, 0.25), na.rm = FALSE,
                        names = TRUE, type = 7, ...) {
  # TODO: functional quantiles will need (a lot) more thought,
  # cf. Serfling, R., & Wijesuriya, U. (2017).
  # Depth-based nonparametric description of functional data, with emphasis on use of spatial depth.
  warning("only pointwise, non-functional quantiles implemented for tfs.")
  summarize_tf(x,
    probs = probs, na.rm = na.rm,
    names = names, type = type, op = "quantile", eval = is_tfd(x), ...
  )
}

#' @inheritParams stats::sd
#' @export
#' @rdname tfsummaries
sd <- function(x, na.rm = FALSE) UseMethod("sd")

#' @importFrom stats sd
#' @rdname tfsummaries
#' @export
sd.default <- stats::sd

#' @export
#' @rdname tfsummaries
sd.tf <- function(x, na.rm = FALSE) {
  summarize_tf(x, na.rm = na.rm, op = "sd", eval = is_tfd(x))
}

#' @inheritParams stats::var
#' @export
#' @rdname tfsummaries
var <- function(x, y = NULL, na.rm = FALSE, use) UseMethod("var")

#' @export
#' @importFrom stats sd
#' @rdname tfsummaries
var.default <- stats::var

#' @export
#' @rdname tfsummaries
var.tf <- function(x, y = NULL, na.rm = FALSE, use) {
  summarize_tf(x, na.rm = na.rm, op = "sd", eval = is_tfd(x))
}

# cov / cor # needs image class/fpca methods

#' @param object a `tfd` object
#' @export
#' @rdname tfsummaries
summary.tf <- function(object, ...) {
  tf_depths <- tf_depth(object, ...)
  central <- which(tf_depths <= median(tf_depths))
  c(
    mean = mean(object), var = var(object),
    median = object[which.max(tf_depths)],
    central_half = range(object[central])
  )
}
