#' Eigenfunctions via weighted SVD
#' 
#' Compute (truncated) orthonormal eigenfunctions and scores
#' for data potentially on a non-equidistant grid.
#' @param data numeric matrix of function evaluations (each row is one curve, no NAs)
#' @param arg numeric vector of argument values
#' @param pve percentage of variance explained
#' @return a list with entries
#' - `mu`` estimated mean function (numeric vector)
#' - `efunctions`` estimated FPCs (numeric matrix, columns represent FPCs)
#' - `scores` estimated FPC scores (one row per observed curve)
#' - `npc` how many FPCs were returned for the given `pve` (integer)
#' @references code adapted from / inspired by `wsvd()` function of Bioconductor
#'   package `mogsa` by Cheng Meng. 
# code adapted from mogsa::wsvd by Cheng Meng
fpc_wsvd <- function(data, arg, pve = .995) {
  UseMethod("fpc_wsvd")
}
  
fpc_wsvd.matrix <- function(data, arg, pve = .995) {  
  assert_matrix(data, mode = "numeric", any.missing = FALSE, 
                min.cols = 2, min.rows = 1)
  assert_numeric(arg, any.missing = FALSE, sorted = TRUE, len = ncol(data))
  assert_number(pve, lower = 0, upper = 1)

  delta <- c(0, diff(arg))
  # trapezoid integration weights:
  w <- .5 * c(delta[-1] + head(delta, -1), tail(delta, 1))
  mean <- colMeans(data)
  data_wc <- t((t(data) - mean) * sqrt(w))
  pc <- svd(data_wc, nu = 0, nv = min(dim(data)))
  pve_observed <- cumsum(pc$d^2) / sum(pc$d^2)
  use <- min(which(pve_observed >= pve))
  efunctions <- pc$v[, 1:use] / sqrt(w)
  evalues <- pc$d[1:use]
  scores <- t(qr.coef(qr(efunctions), t(data_wc) / sqrt(w))) #!!
  list(
    mu = mean, efunctions = efunctions,
    scores = scores, npc = use, evalues = evalues
  )
}
fpc_wsvd.data.frame <- function(data, arg, pve = .995) { 
  data_mat <- df_2_mat(data)
  fpc_wsvd.matrix(data_mat, arg = attr(data_mat, "arg"), pve = pve)
}

#---------------------------------------------------------------------------

fpc_wrapper <- function(efunctions) {
  function(arg) {
    t(efunctions[, arg, interpolate = TRUE, matrix = TRUE])
  }
}
