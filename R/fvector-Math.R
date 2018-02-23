fun_math <- function(x, op){
  attr_ret <- attributes(x)
  ret <- map(x, ~ do.call(op, list(x = .x)))
  attributes(ret) <- attr_ret
  if (is_feval(ret)) {
    forget(attr(ret, "evaluator"))
  }  
  return(ret)
}

Math.fvector <- function(x) {
  not_defined <- switch(.Generic, 
    `cumprod` = , `cumsum` = TRUE, FALSE)
  if (not_defined) 
    stop(sprintf("%s not defined for \"fvector\" objects", .Generic))
}  
Math.feval <- function(x) {
  NextMethod()
  fun_math(x, .Generic)
}  
Math.fbase <- function(x) {
  #TODO cummin, cummax should yield monotonous
  basis_args <- attr(x, "basis_args")
  eval <- fun_math(feval(x), .Generic)
  do.call("fbase", 
    c(list(eval), basis_args, penalized = FALSE, verbose = FALSE))
}  
Math2.feval <- function(x) {
  NextMethod()
  fun_math(x, .Generic)
}  
Math2.fbase <- function(x) {
  #TODO cummin, cummax should yield monotonous
  basis_args <- attr(x, "basis_args")
  eval <- fun_math(feval(x), .Generic)
  do.call("fbase", 
    c(list(eval), basis_args, penalized = FALSE, verbose = FALSE))
}  

#-------------------------------------------------------------------------------

Summary.fvector <- function(...) {
  not_defined <- switch(.Generic, 
    `all` = , `any` = TRUE, FALSE)
  if (not_defined) 
    stop(sprintf("%s not defined for \"fvector\" objects", .Generic))
}  

fun_summary <- function(f, op){
  attr_ret <- attributes(f)
  m <- as.matrix(f)
  ret <- apply(m, 2, op)
  if (is_feval(f)) {
    forget(attr_ret$evaluator)
  }  
  return(list(ret, list(attr_ret))
}


