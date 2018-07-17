#' Random generator for Gaussian Processes
#' 
#' The function generates `n` realizations of Gaussian processes , either with
#' squared exponential covariance \eqn{Cov(x(t), x(t')) = \exp(-(t'-t)^2)/s) + n
#' \delta_{t}(t')} or Wiener process covariance \eqn{Cov(x(t), x(t')) =
#' \min(t',t)/s + n \delta_{t}(t')} with `scale` parameter s and `nugget` effect
#' n.
#'
#' @param n how many realizations to draw
#' @param argvals vector of evaluation points (`argvals` of the return object).
#'   Defaults to (0, 0.02, 0.04, ..., 1).
#' @param scale scale parameter (see Description). Defaults to the width of the
#'   domain divided by 10.
#' @param cor type of correlation structure to use. Currently available:
#'   `"squareexp"` or `"wiener"`, see Description.
#' @param nugget nugget effect for additional white error noise. Defaults to
#'   `scale/200` (so: very little noise)
#' @return an `feval`-vector of length `n`
#' @importFrom mvtnorm rmvnorm
#' @export
rgp <- function(n, argvals = seq(0, 1, l = 51), scale = diff(range(argvals))/10, 
  cor = c("squareexp", "wiener"), nugget = scale/200) {
  cor <- match.arg(cor)
  f_cov <- switch(cor, "wiener" = function(s, t) pmin(s, t)/scale,
    "squareexp" = function(s,t) exp(-(s - t)^2/scale))
  cov <- outer(argvals, argvals, f_cov) + diag(0*argvals + nugget)
  y <- rmvnorm(n, mean = 0 * argvals, sigma = cov)
  feval(y, argvals = argvals)
}

#' @importFrom stats runif
jiggle <- function(f, ...) {
  stopifnot(is_feval(f))
  f <- as.feval_irreg(f)
  jiggle_args <- function(argvals) {
    diffs <- diff(argvals)
    n <- length(argvals)
    jiggle <- runif(n - 2, -.49, +.49) * diffs[2 : (n - 1)]
    new_args <- argvals[2 : (n - 1)] + jiggle
    c(runif(1, argvals[1], new_args[1]), new_args, 
      runif(1, new_args[n - 2], argvals[n]))
  } 
  new_args <- map(argvals(f), jiggle_args)
  feval(map2(new_args, evaluations(f), cbind), domain = domain(f))
}
