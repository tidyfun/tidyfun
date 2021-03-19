#' Gaussian Process random generator
#'
#' The function generates `n` realizations of a Gaussian process, either with
#' squared exponential covariance \eqn{Cov(x(t), x(t')) = \exp(-(t'-t)^2)/s) + n
#' \delta_{t}(t')}
#' or Wiener process covariance \eqn{Cov(x(t), x(t')) =
#' \min(t',t)/s + n \delta_{t}(t')}
#' with `scale` parameter s and `nugget` effect n.
#'
#' @param n how many realizations to draw
#' @param arg vector of evaluation points (`arg` of the return object).
#'   Defaults to (0, 0.02, 0.04, ..., 1). If a single **integer** (don't forget the `L`...), creates a grid
#'   of the given length over (0,1).
#' @param scale scale parameter (see Description). Defaults to the width of the
#'   domain divided by 10.
#' @param cor type of correlation structure to use. Currently available:
#'   `"squareexp"` or `"wiener"`, see Description.
#' @param nugget nugget effect for additional white noise / unstructured variability.
#'  Defaults to `scale/200` (so: very little noise).
#' @return an `tfd`-vector of length `n`
#' @importFrom mvtnorm rmvnorm
#' @export
tf_rgp <- function(n, arg = 51L, scale = diff(range(arg)) / 10,
                   cor = c("squareexp", "wiener"), nugget = scale / 200) {
  cor <- match.arg(cor)
  if (length(arg) == 1) {
    check_integerish(arg, lower = 1)
    arg <- seq(0, 1, length = arg)
  } 
  check_numeric(arg, any.missing = FALSE, unique = TRUE)
  check_number(n, lower = 1)
  check_number(scale, lower = 0)
  check_number(nugget, lower = 0)

  f_cov <- switch(cor, "wiener" = function(s, t) pmin(s, t) / scale,
    "squareexp" = function(s, t) exp( -(s - t)^2 / scale)
  )
  cov <- outer(arg, arg, f_cov) + diag(0 * arg + nugget)
  y <- rmvnorm(n, mean = 0 * arg, sigma = cov)
  tfd(y, arg = arg)
}

#' Make a `tf` (more) irregular
#'
#' Randomly create some irregular functional data from regular ones.  
#' **jiggle** it by randomly moving around its `arg`-values. Only for `tfd`.
#' **sparsify** it by setting (100*`dropout`)% of its values to `NA`.
#'
#' @param f a `tfd` object
#' @param amount how far away from original grid points can the new
#' grid points lie, at most (relative to original distance to neighboring grid points).
#' Defaults to at most 40% (0.4) of the original grid distances. Must be lower than 0.5
#' @importFrom stats runif
#' @export
#' @rdname tf_jiggle
tf_jiggle <- function(f, amount = .4, ...) {
  stopifnot(is_tfd(f))
  assert_number(amount, lower = 0, upper = .5)
  f <- as.tfd_irreg(f)
  new_args <- map(tf_arg(f), tf_jiggle_args, amount = amount)
  tfd(map2(new_args, tf_evaluations(f), cbind), domain = tf_domain(f))
}
tf_jiggle_args <- function(arg, amount) {
  diffs <- diff(arg)
  n <- length(arg)
  
  push_left_right <- sample(c(-1, 1), n - 2, replace = TRUE)
  use_diffs <- ifelse(push_left_right == -1, 
                      diffs[1:(n - 2)], #push left at most 40% of distance to left
                      diffs[2:(n - 1)]) #push right at most 40% of distance to left
  tf_jiggle <- runif(n - 2, 0, amount) * use_diffs * push_left_right
  new_args <- arg[2:(n - 1)] + tf_jiggle
  c(
    runif(1, arg[1], new_args[1]), new_args,
    runif(1, new_args[n - 2], arg[n])
  )
}

#' @rdname tf_jiggle
#' @param dropout how many values of `f` to drop, defaults to 50%.
#' @param ... not used currently
#' @export
tf_sparsify <- function(f, dropout = .5, ...) {
  stopifnot(is_tf(f))
  nas <- map(
    tf_evaluations(f),
    ~ifelse(runif(length(.x)) < dropout, TRUE, FALSE)
  )
  tf_evals <- map2(
    tf_evaluations(f), nas,
    ~ .x[!.y]
  )
  tf_args <- ensure_list(tf_arg(f))
  tf_args <- map2(
    tf_args, nas,
    ~ .x[!.y]
  )
  tfd.list(tf_evals, tf_args, resolution = attr(f, "resolution"), 
      domain = tf_domain(f))
}
