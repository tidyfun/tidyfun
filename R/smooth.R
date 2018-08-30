#' @title Simple smoothing of `tf` objects
#'
#' @description Apply running means or medians, `lowess` or Savitzky-Golay
#'   filtering to smooth functional data. This does nothing for `tfb`-objects,
#'   which should be smoothed by using a smaller basis / stronger penalty.\cr
#'   The [stats::smooth()] function is re-exported as the default method.
#'
#' @details `smooth.tfd` overrides/automatically sets some defaults of the used
#'   methods: 
#'   
#'   - **`lowess`** uses a span parameter of `f` = .15 (instead of .75)
#'   by default. 
#'   - **`rollmean`/`median`** use a window size of `k` = <number of
#'   grid points>/20 (i.e., the nearest odd integer to that) and sets `fill=
#'   "extend"` (i.e., constant extrapolation to replace missing values at the
#'   extremes of the domain) by default. Use `fill= NA` for `zoo`'s default
#'   behavior of shortening the smoothed series. 
#'   - **`savgol`** uses a window size of `k` = <number of
#'   grid points>/10 (i.e., the nearest odd integer to that).
#'
#' @param x a `tf` object containing functional data
#' @param method one of "lowess" (see [stats::lowess()]), "rollmean",
#'   "rollmedian" (see [zoo::rollmean()]) or "sgolay" (see [pracma::savgol()])
#' @param ... arguments for the respective `method`. See Details.
#' @return a smoothed version of the input. For some methods/options, the
#'   smoothed functions may be shorter than the original ones (at both ends).
#' @importFrom stats smooth
#' @export
smooth <- function(x, ...) {
  UseMethod("smooth")
}
#' @export
#' @rdname smooth
smooth.tfb <- function(x, ...) {
  message("just use a smaller base / more penalty....")
  x
}
#' @importFrom stats lowess
#' @importFrom zoo rollmean rollmedian
#' @importFrom pracma savgol
#' @importFrom stats lowess
#' @importFrom stringr str_detect
#' @rdname smooth
#' @export
#' @examples
#' f <- tf_sparsify(tf_jiggle(tf_rgp(4, 201L, nugget = .05)))
#' f_lowess <- smooth(f, "lowess")
#' # these methods ignore the distances between arg-values:
#' f_mean <- smooth(f, "rollmean")
#' f_median <- smooth(f, "rollmean", k = 31)
#' f_sg <- smooth(f, "savgol", fl = 31)
#' layout(t(1:4))
#' plot(f, points = FALSE)
#' plot(f_lowess, points = FALSE)
#' lines(smooth(f, "lowess", f = .9), col = 2, alpha= .2)
#' plot(f_mean, points = FALSE)
#' lines(f_median, col = 2, alpha= .2) # note constant extrapolation
#' plot(f_sg, points = FALSE) 
smooth.tfd <- function(x, method = c("lowess", "rollmean", "rollmedian", "savgol"),
  ...) {
  method <- match.arg(method)
  smoother <- get(method, mode = "function")
  dots <- list(...)
  if (any(str_detect(method, c("savgol", "rollm")))) {
    if (!is_equidist(x)) {
      warning("non-equidistant arg-values in ", sQuote(deparse(substitute(x))),
        " ignored by ", method, ".")
    }
    if (str_detect(method, "rollm")) {
      if (is.null(dots$k)) {
        dots$k <- ceiling(.05 * min(tf_count(x)))
        dots$k <- dots$k + !(dots$k %% 2) # make uneven
        message("using k = ", dots$k, " observations for rolling data window.")
      }
      if (is.null(dots$fill)) {
        message("setting fill = 'extend' for start/end values.")
        dots$fill <- "extend"
      }
    }
    if (str_detect(method, "savgol")) {
      if (is.null(dots$fl)) {
        dots$fl <- ceiling(.15 * min(tf_count(x)))
        dots$fl <- dots$fl + !(dots$fl %% 2) # make uneven
        message("using fl = ", dots$fl, " observations for rolling data window.")
      }
    }
    smoothed <- map(tf_evaluations(x), ~ do.call(smoother, append(list(.x), dots)))
  }
  if (str_detect(method, "lowess")) {
    if (is.null(dots$f)) {
      dots$f <- .15
      message("using f = ", dots$f, " as smoother span for lowess")
    }
    smoothed <- map(tf_evaluations(x), ~ do.call(smoother, append(list(.x), dots))$y)
  }
  tfd(smoothed, arg(x), evaluator = !!attr(x, "evaluator_name"), 
    resolution = attr(x, "resolution"), domain = domain(x))
}
#' @rdname smooth
#' @export
#' @inheritParams stats::smooth
smooth.default <- function(x, ..., 
  kind = c("3RS3R", "3RSS", "3RSR", "3R", "3", "S"), 
  twiceit = FALSE, endrule = c("Tukey", "copy"), do.ends = FALSE) {
  kind <- match.arg(kind)
  endrule <- match.arg(endrule)
  stats::smooth(x, kind = kind, twiceit = twiceit, endrule = endrule, 
    do.ends = do.ends) 
}
  
