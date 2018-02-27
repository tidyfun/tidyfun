fun_math <- function(x, op){
  attr_ret <- attributes(x)
  ret <- map(x, ~ do.call(op, list(x = .x)))
  attributes(ret) <- attr_ret
  if (is_feval(ret)) {
    forget(attr(ret, "evaluator"))
  }  
  return(ret)
}

#' Math, Summary and Ops Methods for `fvector`
#' 
#' These define methods and operators that mostly work `argval`-wise on
#' `fvector` objects. See [?Math], [?Summary], [?Ops].
#' See examples below. Equality checks of functional objects are rather iffy and
#' not very reliable at this point. Note that `max` and `min` are not guaranteed 
#' to be maximal/minmal over the entire domain, only on the evaluation grid used for
#' computation. With the exception of addition and multiplication, 
#' operations on `fbase`-objects first evaluate them over their `argvals`,
#' perform computations on these evaluations and then convert back to an `fbase`-
#' object, so a loss of precision should be expected, especially so if bases are small
#' or data is very wiggly.
#' 
#' 
#' @param x an `fvector`
#' @param ... `fvector`-objects
#' @param e1 an `fvector` or a numeric vector
#' @param e2 an `fvector` or a numeric vector
#' 
#' @rdname fvectorgroupgenerics
#' @examples 
#' set.seed(1859)
#' f <- rgp(4)
#' 2 * f == f + f
#' sum(f) == f[1] + f[2] + f[3] + f[4]
#' log(exp(f)) == f 
#' plot(f, points = FALSE); lines(range(f), col = 2, lty = 2)
#' 
#' f2 <- fbase(rgp(5), k = 50)
#' layout(t(1:2)); plot(f2, col = 1:5); plot(cummax(f2), col = 1:5); lines(f2)
#' 
#' # loss of precision:
#' f3 <- fbase(rgp(5, scale = 0.01), k = 50)
#' log(exp(f3)) == f3 #!!
#' plot(log(exp(f3))); lines(f3, lty = 2, col = 2) # still reasonable
#' 
Math.feval <- function(x) {
  fun_math(x, .Generic)
} 
#' @rdname fvectorgroupgenerics
Math.fbase <- function(x) {
  basis_args <- attr(x, "basis_args")
  eval <- fun_math(feval(x), .Generic)
  do.call("fbase", 
    c(list(eval), basis_args, penalized = FALSE, verbose = FALSE))
}  

#' @rdname fvectorgroupgenerics
cummax.feval <- function(...) {
  summarize_fvector(..., op = "cummax", eval  = TRUE)
}
#' @rdname fvectorgroupgenerics
cummin.feval <- function(...) {
  summarize_fvector(..., op = "cummin", eval  = TRUE)
}
#' @rdname fvectorgroupgenerics
cumsum.feval <- function(...) {
  summarize_fvector(..., op = "cumsum", eval  = TRUE)
}
#' @rdname fvectorgroupgenerics
cumprod.feval <- function(...) {
  summarize_fvector(..., op = "cumprod", eval  = TRUE)
}

#' @rdname fvectorgroupgenerics
cummax.fbase <- function(...) {
  summarize_fvector(..., op = "cummax", eval  = FALSE)
}
#' @rdname fvectorgroupgenerics
cummin.fbase <- function(...) {
  summarize_fvector(..., op = "cummin", eval  = FALSE)
}
#' @rdname fvectorgroupgenerics
cumsum.fbase <- function(...) {
  summarize_fvector(..., op = "cumsum", eval  = FALSE)
}
#' @rdname fvectorgroupgenerics
cumprod.fbase <- function(...) {
  summarize_fvector(..., op = "cumprod", eval  = FALSE)
}

#-------------------------------------------------------------------------------

#' @rdname fvectorgroupgenerics
Summary.fvector <- function(...) {
  not_defined <- switch(.Generic, 
    `all` = , `any` = TRUE, FALSE)
  if (not_defined) 
    stop(sprintf("%s not defined for \"fvector\" objects", .Generic))
  summarize_fvector(..., op = .Generic, eval  = is_feval(list(...)[[1]]))
}  

#-------------------------------------------------------------------------------
# TODO:
# inner product ?
#`%*%.default` = .Primitive("%*%") # assign default as current definition
#`%*%` = function(x,...){ #make S3
#  UseMethod("%*%",x)
#}
# `%*%.fvector(x, y) = [int x_i(t)*y_i(t) dt] 




