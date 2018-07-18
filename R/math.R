# utility function for linear operations that can be done on coefs or evaluations directly.
fun_math <- function(x, op){
  attr_ret <- attributes(x)
  ret <- map(evaluations(x), ~ do.call(op, list(x = .x)))
  forget(attr_ret$evaluator)
  if (is_irreg(x)) {
    ret <- map2(argvals(x), ret, ~ list(argvals = .x, data = .y))
  } 
  attributes(ret) <- attr_ret
  ret
}
#-------------------------------------------------------------------------------
# used for Summary grup generics and stats-methods...
# op has to be a string!
summarize_fvector <- function(..., op = NULL, eval = FALSE) {
  dots <- list(...)
  funs <- map_lgl(dots, is_fvector)
  op_args <- dots[!funs]
  funs <- dots[funs]
  op_call <- function(x) do.call(op, c(list(x), op_args))
  funs <- do.call(c, funs)
  attr_ret <- attributes(funs)
  m <- as.matrix(funs)
  ret <- apply(m, 2, op_call)
  argvals <- as.numeric(colnames(m))
  args <- c(list(ret), argvals = list(argvals),
    domain = list(domain(funs)), 
    signif = attr(funs, "signif_argvals"))
  if (eval) {
    return(do.call(feval, c(args, evaluator = as.name(attr(funs, "evaluator_name")))))
  } else {
    return(do.call(fbase, c(args, penalized = FALSE, attr(funs, "basis_args"))))
  }
}
#------------------------------------------------------------------------------

#' Math, Summary and Ops Methods for `fvector`
#' 
#' These define methods and operators that mostly work `argval`-wise on
#' `fvector` objects, see `?groupGeneric` for implementation details.
#' 
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
#' @param ... `fvector`-objects (not used for `Math` group generic)
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
#' @export
#' 
Math.feval <- function(x, ...) {
  fun_math(x, .Generic)
} 
#' @rdname fvectorgroupgenerics
#' @export
Math.fbase <- function(x, ...) {
  basis_args <- attr(x, "basis_args")
  eval <- fun_math(feval(x), .Generic)
  do.call("fbase", 
    c(list(eval), basis_args, penalized = FALSE, verbose = FALSE))
}  

#' @rdname fvectorgroupgenerics
#' @export
cummax.feval <- function(...) {
  summarize_fvector(..., op = "cummax", eval  = TRUE)
}
#' @rdname fvectorgroupgenerics
#' @export
cummin.feval <- function(...) {
  summarize_fvector(..., op = "cummin", eval  = TRUE)
}
#' @rdname fvectorgroupgenerics
#' @export
cumsum.feval <- function(...) {
  summarize_fvector(..., op = "cumsum", eval  = TRUE)
}
#' @rdname fvectorgroupgenerics
#' @export
cumprod.feval <- function(...) {
  summarize_fvector(..., op = "cumprod", eval  = TRUE)
}
#' @rdname fvectorgroupgenerics
#' @export
cummax.fbase <- function(...) {
  summarize_fvector(..., op = "cummax", eval  = FALSE)
}
#' @rdname fvectorgroupgenerics
#' @export
cummin.fbase <- function(...) {
  summarize_fvector(..., op = "cummin", eval  = FALSE)
}
#' @rdname fvectorgroupgenerics
#' @export
cumsum.fbase <- function(...) {
  summarize_fvector(..., op = "cumsum", eval  = FALSE)
}
#' @rdname fvectorgroupgenerics
#' @export
cumprod.fbase <- function(...) {
  summarize_fvector(..., op = "cumprod", eval  = FALSE)
}

#-------------------------------------------------------------------------------

#' @rdname fvectorgroupgenerics
#' @export
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




