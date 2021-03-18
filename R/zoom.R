#' Functions to zoom in/out on functions
#'
#' These are used to redefine or restrict the `domain` of `tf` objects.
#'
#' @param f a `tf`-object
#' @param begin numeric vector of length 1 or `length(f)`.
#'  Defaults to the lower limit of the domain of `f`.
#' @param end numeric vector of length 1 or `length(f)`.
#'  Defaults to the upper limit of the domain of `f`.
#' @param ... not used
#' @return an object like `f` on a new domain (potentially).
#' Note that regular functional data and functions in basis representation will
#'   be turned into irregular `tfd`-objects iff `begin` or `end` are not scalar.
#' @export
#' @examples
#'   (x <- tf_rgp(10))
#'   plot(x)
#'   tf_zoom(x, .5, .9)
#'   lines(tf_zoom(x, .5, .9), col = "red")
#'   points(tf_zoom(x, seq(0, .5, l = 10), seq(.5, 1, l = 10)), col = "blue")
tf_zoom <- function(f, begin, end, ...) {
  UseMethod("tf_zoom")
}

prep_tf_zoom_args <- function(f, begin, end) {
  assert_numeric(begin, any.missing = FALSE, min.len = 1, max.len = length(f))
  assert_numeric(end, any.missing = FALSE, min.len = 1, max.len = length(f))
  regular <- TRUE
  # uses unique to homogenize and check regularity in one go
  if (dplyr::n_distinct(begin) == 1) {
    begin <- rep(begin[1], length(f))
  } else {
    regular <- FALSE
  }
  if (dplyr::n_distinct(end) == 1) {
    end <- rep(end[1], length(f))
  } else {
    regular <- FALSE
  }
  stopifnot(length(begin) == length(end), all(begin < end))
  new_domain <- c(min(begin), max(end))
  list(begin = begin, end = end, dom = new_domain, regular = regular)
}

#' @rdname tf_zoom
#' @export
tf_zoom.tfd <- function(f, begin = tf_domain(f)[1], end = tf_domain(f)[2], 
                        ...) {
  args <- prep_tf_zoom_args(f, begin, end)
  ret <- pmap(
    list(f[, tf_arg(f), matrix = FALSE], args$begin, args$end),
    ~ filter(..1, arg >= ..2 & arg <= ..3)
  )
  if (is_irreg(f) | !args$regular) {
    nas <- map_lgl(ret, ~length(.x$arg) == 0)
    if (all(nas)) stop("no data in zoom region.")
    if (any(nas)) warning("NAs created by tf_zoom.")
    for (n in which(nas)) ret[[n]] <- data.frame(arg = unname(args$dom[1]), value = NA_real_)
  } else {
    if (any(map_lgl(ret, ~length(.x$arg) == 0))) {
      stop("no data in zoom region.")
    }
  }
  ret <- tfd(ret, domain = args$dom, resolution = attr(f, "resolution"))
  tf_evaluator(ret) <- attr(f, "evaluator_name")
  ret
}
#' @rdname tf_zoom
#' @export
tf_zoom.tfb <- function(f, begin = tf_domain(f)[1], end = tf_domain(f)[2], 
                        ...) {
  args <- prep_tf_zoom_args(f, begin, end)
  if (!args$regular) {
    message("tf_zoom() with varying start or end points - converting to tfd.")
    return(tf_zoom(tfd(f), begin, end))
  }
  use <- tf_arg(f) >= args$dom[1] & tf_arg(f) <= args$dom[2]
  if (!any(use)) stop("no data in zoom region.")
  ret <- f
  memoise::forget(ret$basis)
  attr(ret, "basis_matrix") <- attr(f, "basis_matrix")[use, ]
  attr(ret, "arg") <- tf_arg(f)[use]
  attr(ret, "domain") <- args$dom
  ret
}

#' @rdname tf_zoom
#' @export
tf_zoom.tfb_fpc <-  function(f, begin = tf_domain(f)[1], end = tf_domain(f)[2], 
                             ...) {
  warning("zoomed-in FPC representation likely to lose orthogonality of FPC basis.")
  NextMethod()  
}
