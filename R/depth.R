#' Functional Data Depth
#'
#' Data tf_depth for functional data.
#' Currently implemented: Modified Band-2 Depth, see reference.
#'
#' @param x `tf` (or a matrix of evaluations)
#' @param depth currently available: "MBD", i.e. modified band depth
#' @param arg grid of evaluation points
#' @param na.rm TRUE remove missing observations?
#' @param ... further arguments handed to the function computing the respective tf_depth.
#' @return vector of tf_depth values
#' @references Sun, Y., Genton, M. G., & Nychka, D. W. (2012).
#'   Exact fast computation of band tf_depth for large functional datasets:
#'   How quickly can one million curves be ranked?. *Stat*, **1**(1), 68-74.
#'   Lopez-Pintado, S. and Romo, J. (2009).
#'   On the Concept of Depth for Functional Data.
#'   *Journal of the American Statistical Association*, **104**, 718-734.
#' @export
#' @rdname tf_depth
tf_depth <- function(x, depth = "MBD", na.rm = TRUE, ...) {
  UseMethod("tf_depth")
}
#' @export
#' @rdname tf_depth
tf_depth.matrix <- function(x, depth = "MBD", na.rm = TRUE,
                            arg = unlist(find_arg(x, NULL)), ...) {
  depth <- match.arg(depth)
  # TODO: this ignores na.rm -- should it?
  switch(depth,
    "MBD" = mbd(x, arg, ...)
  )
}
#' @export
#' @rdname tf_depth
tf_depth.tf <- function(x, depth = "MBD", na.rm = TRUE, arg = NULL, ...) {
  if (!missing(arg)) assert_arg_vector(arg, x)
  # TODO: warn if irreg?
  if (na.rm) x <- x[!is.na(x)]
  tf_depth(as.matrix(x, arg = arg, interpolate = TRUE),
    depth = depth,
    na.rm = na.rm, ...
  )
}

#-------------------------------------------------------------------------------

# modified band-2 tf_depth:
mbd <- function(x, arg = seq_len(ncol(x)), ...) {
  # algorithm of Sun/Genton/Nychka (2012)
  ranks <- apply(x, 2, rank, na.last = "keep", ...)
  weights <- {
    # assign half interval length to 2nd/nxt-to-last points to 1st and last point
    # assign other half intervals to intermediate points
    lengths <- diff(arg) / 2
    (c(lengths, 0) + c(0, lengths)) / diff(range(arg))
  }
  n <- nrow(ranks)
  tmp <- colSums( t( (n - ranks) * (ranks - 1)) * weights, na.rm = TRUE)
  (tmp + n - 1) / choose(n, 2)
}
