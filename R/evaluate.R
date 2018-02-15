evaluate <- function(f, argvals, ...) UseMethod("evaluate")
evaluate.default <- function(f, argvals, ...) .NotYetImplemented()
evaluate.feval <- function(f, argvals) {
  if (!is.list(argvals)) argvals <- list(argvals)
  pmap(list(argvals, attr(f, "argvals"), evaluations(f)), 
    ~ evaluate_once(..1, ..2, ..3, attr(f, "evaluator")))
}  

evaluate_once <- function(x, evaluations, argvals, evaluator) {
  if (isTRUE(all.equal(x, argvals))) return(evaluations)
  stopifnot(all(!duplicated(x)))
  evaluator(x, argvals = argvals, evaluations = evaluations)
}

# 
# evaluate.feval_reg <- function(f, argvals, ...) {
#   argvals <- map(argvals, ~ signif(., attr(f,"signif_argvals")))
#   if (isTRUE(all.equal(v, .argvals))) return(.data)
#   stopifnot(all(!duplicated(v)))
#   v_arg <- sort(unique(c(v, .argvals)))
#   v_arg_match <- match(v_arg, .argvals, nomatch = length(.argvals) + 1)
#   requested <-  v_arg %in% v
#   coredata(interpolator(zoo(.data[v_arg_match], v_arg)))[requested]
# }
