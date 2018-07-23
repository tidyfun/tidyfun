library(devtools)
library(testthat)
library(checkmate)
library(dplyr)
library(purrr)

load_all(".")
plot(f <- rgp(10, 101L, nugget = .1))

#' @importFrom stats smooth
#' @exportMethod 
smooth <- function(x, ...) {
  UseMethod(smooth)
}
#' @export
smooth.default <- stats::smooth 
#' @export
smooth.tfb <- function(x, method = c("runmed", "roll"), ...) {
  message("just use a smaller base / more penalty....")
  x
}

x.Date <- as.Date(paste(2004, rep(1:4, 4:1), sample(1:28, 10), sep = "-"))
x <- zoo(rnorm(12), x.Date)
fi <- tfd(coredata(x), arg = as.numeric(index(x)), signif = 5)
rollmean(zoo(coredata(x), 1:12), 2)
rollmean(x, 2)

#' @importFrom stats lowess
#' @importFrom zoo rollmean rollmedian
#' @importFrom KernSmooth locpoly
smooth.tfd <- function(x, kind = c("rollmean", "rollmedian", "lowess", "locpoly"), ...) {
  kind <- match.arg(kind)
  smoother <- get(kind, mode = "function")
  # todo : warn about irregular grids for roll*, only use eval
  if(stringr::str_detect(kind, "rollm")) {
    # if(!is_equidist(x)) warning("uhoh")
    # modify dots: needs k
    smoothed <- map(evaluations(x), ~ smoother(.x, ...))
  } else {
    # locpoly needs bandwidth
    smoothed <- map2(evaluations(x), ensure_list(arg(x)), ~ smoother(.y, .x, ...)$y)
  }
  tfd(smoothed, arg(x), evaluator = evaluator(x), signif = attr(x, "signif"))
}
