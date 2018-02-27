fun_math <- function(x, op){
  attr_ret <- attributes(x)
  ret <- map(x, ~ do.call(op, list(x = .x)))
  attributes(ret) <- attr_ret
  if (is_feval(ret)) {
    forget(attr(ret, "evaluator"))
  }  
  return(ret)
}
Math.feval <- function(x) {
  fun_math(x, .Generic)
}  
Math.fbase <- function(x) {
  basis_args <- attr(x, "basis_args")
  eval <- fun_math(feval(x), .Generic)
  do.call("fbase", 
    c(list(eval), basis_args, penalized = FALSE, verbose = FALSE))
}  

cummax.feval <- function(...) {
  summarize_fvector(..., op = "cummax", eval  = TRUE)
}
cummin.feval <- function(...) {
  summarize_fvector(..., op = "cummin", eval  = TRUE)
}
cumsum.feval <- function(...) {
  summarize_fvector(..., op = "cumsum", eval  = TRUE)
}
cumprod.feval <- function(...) {
  summarize_fvector(..., op = "cumprod", eval  = TRUE)
}

cummax.fbase <- function(...) {
  summarize_fvector(..., op = "cummax", eval  = FALSE)
}
cummin.fbase <- function(...) {
  summarize_fvector(..., op = "cummin", eval  = FALSE)
}
cumsum.fbase <- function(...) {
  summarize_fvector(..., op = "cumsum", eval  = FALSE)
}
cumprod.fbase <- function(...) {
  summarize_fvector(..., op = "cumprod", eval  = FALSE)
}



#-------------------------------------------------------------------------------

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




