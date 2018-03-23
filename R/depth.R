#' Data Depth
#' 
#' Data depth for functional data. Currently implemented: Modified Band Depth
#' 
#' @param x `fvector` (or a matrix of evaluations)
#' @param depth currently available: "MBD", i.e. modified band depth
#' @param argvals grid of evaluation points
#'
#' @return vector of depth values
#' @references Sun, Y., Genton, M. G., & Nychka, D. W. (2012). 
#'   Exact fast computation of band depth for large functional datasets: 
#'   How quickly can one million curves be ranked?. *Stat*, **1**(1), 68-74.
#'   Lopez-Pintado, S. and Romo, J. (2009). 
#'   On the Concept of Depth for Functional Data.
#'   *Journal of the American Statistical Association*, **104**, 718-734.
#' @export
#' @rdname depth
depth <- function(x, depth = "MBD", ...) {
  UseMethod("depth")
}
#' @export
#' @rdname depth
depth.matrix <- function(x, depth = "MBD", argvals = seq_len(ncol(x)), ...) {
  depth <- match.arg(depth)
  switch(depth,
    "MBD" = mbd(x, argvals, ...))
}
#' @export
#' @rdname depth
depth.fvector <- function(x, depth = "MBD", argvals = tidyfun::argvals(x), ...) {
  depth(as.matrix(x, argvals = argvals, interpolate = TRUE), argvals = argvals, 
    depth = depth)
}  
# modified band depth:
mbd <- function(x, argvals = seq_len(ncol(x)), ...) {
  # algorithm of Sun/Genton/Nychka (2012)
  ranks <- apply(x, 2, rank, na.last = NA, ...)
  weights <- {
    #assign half interval length to 2nd/nxt-to-last points to 1st and last point
    #assign other half intervals to intermediate points
    lengths <- diff(argvals)/2
    (c(lengths, 0) + c(0, lengths)) / diff(range(argvals))
  }
  n <- nrow(ranks)
  tmp <- colSums(t((n - ranks ) * (ranks - 1)) * weights)
  (tmp + n - 1)/choose(n, 2) 
} 


