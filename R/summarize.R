#' Functions that summarize `tf` objects
#' 
#' These will return a `tf` object containing the respective functional statistic.
#' `summary` returns a vector with the mean function, the variance function, and the 
#' functional range of the central half of the functions, as defined by the functional 
#' depth that was usedb (i.e., the band defined by the 50% deepest functions). 
#' 
#' @param x a `tf` object
#' @param ... additional arguments to the respective (pointwise or depth) 
#'   functions, see source code.
#' @name fvectorsummaries
NULL

#' @export 
#' @rdname fvectorsummaries
mean.fvector <- function(x, ...){
  summarize_fvector(x, op = "mean", eval  = is_feval(x), ...)
}

#' @param depth method used to determine the most central element in `x`, i.e., the median.
#'  One of the functional data depths available via [depth()] or `"pointwise"` for
#'  a pointwise median function. 
#' @importFrom stats median
#' @export
#' @rdname fvectorsummaries
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
    med <- x[depths == max(depths)]
    if (length(med) == 1) {
      med
    } else {
      warning(length(med), 
        " observations with maximal depth, returning their mean.")
      mean(med)
    }  
  }
}

#' @importFrom stats quantile
#' @inheritParams stats::quantile
#' @export
#' @rdname fvectorsummaries
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
#' @rdname fvectorsummaries
sd <- function(x, na.rm = FALSE) UseMethod("sd")

#' @importFrom stats sd
#' @rdname fvectorsummaries
sd.default <- stats::sd

#' @export
#' @rdname fvectorsummaries
sd.fvector <- function(x, na.rm = FALSE){
  summarize_fvector(x, na.rm = na.rm, op = "sd", eval  = is_feval(x))
} 

#' @inheritParams stats::var
#' @export
#' @rdname fvectorsummaries
var <- function(x, y = NULL, na.rm = FALSE, use) UseMethod("var")

#' @export
#' @importFrom stats sd
#' @rdname fvectorsummaries
var.default <- stats::var

#' @export
#' @rdname fvectorsummaries
var.fvector <- function(x, y = NULL, na.rm = FALSE, use){
  summarize_fvector(x, na.rm = na.rm, op = "sd", eval  = is_feval(x))
} 

# cov / cor # needs image class/fpca methods

#' @param object a `tfd` object
#' @export
#' @rdname fvectorsummaries
summary.fvector <- function(object, ...) {
  depths <- depth(object, ...)
  central <- which(depths <= median(depths))
  c(mean = mean(object), var = var(object),
    median = object[which.max(depths)], 
    central_half = range(object[central]))
}
