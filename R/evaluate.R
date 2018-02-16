evaluate <- function(f, argvals, ...) UseMethod("evaluate")
evaluate.default <- function(f, argvals, ...) .NotYetImplemented()

evaluate.feval <- function(f, argvals) {
  if (!is.list(argvals)) argvals <- list(argvals)
  assert_argvals(argvals, f)
  pmap(list(argvals, attr(f, "argvals"), evaluations(f)), 
    ~ evaluate_feval_once(x = ..1, argvals = ..2, evaluations = ..3, 
        evaluator = attr(f, "evaluator")))
}  

evaluate_feval_once <- function(x, argvals, evaluations, evaluator) {
  if (isTRUE(all.equal(x, argvals))) return(evaluations)
  evaluator(x, argvals = argvals, evaluations = evaluations)
}

