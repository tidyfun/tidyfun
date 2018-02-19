#' Evaluate `fvector`s of a `data.frame`
#' 
#' The `data.frame` method evaluates `fvector`-columns into list columns of 
#' smaller `data.frames` containing the functions' argvals and evaluations. 
#' For the  `data.frame` method, `argvals` can be a list of `argvals`-vector 
#' used as the `argvals` argument for the [evaluate()]-method for the respective
#' `fvector`-columns in `object`.
#' @param object an `fvector` or a `data.frame`-like object with `fvector` columns
#' @param argvals optional evaluation grid, defaults to `argvals(object)`. 
#' @export
#' @md
evaluate <- function(object, argvals, ...) UseMethod("evaluate")

evaluate.default <- function(object, argvals, ...) .NotYetImplemented()

#' @export
#' @rdname evaluate
evaluate.feval <- function(object, argvals, ...) {
  if (missing(argvals) | is.null(argvals)) argvals <- tidyfun::argvals(object)
  if (!is.list(argvals)) argvals <- list(argvals)
  assert_argvals(argvals, object)
  pmap(list(argvals, attr(object, "argvals"), evaluations(object)), 
    ~ evaluate_feval_once(x = ..1, argvals = ..2, evaluations = ..3, 
        evaluator = attr(object, "evaluator")))
}  

evaluate_feval_once <- function(x, argvals, evaluations, evaluator) {
  if (isTRUE(all.equal(x, argvals))) return(evaluations)
  evaluator(x, argvals = argvals, evaluations = evaluations)
}

#' @rdname evaluate
#' @param ... optional: names of the `fvector`-columns to unnest 
#' @import tidyr
#' @importFrom tidyselect vars_select quos
#' @export
#' @md
evaluate.data.frame <- function(object, argvals, ...) {
  quos <- quos(...)
  # figure out which fvector columns to evaluate
  fvector_cols <- names(object)[map_lgl(object, is_fvector)]
  if (!is_empty(quos)) {
    fvector_cols <- intersect(fvector_cols, map_chr(quos, quo_text))
  }
  if (!length(fvector_cols)) {
    warning("No fvectors to evaluate. Returning unchanged object.")
    return(object)
  }
  if (!missing(argvals)) {
    argvals <- ensure_list(argvals) 
  } else {
    argvals <- map(object[fvector_cols], ~tidyfun::argvals(.))
  }
  # convert them to list-columns of data.frames
  object[fvector_cols] <- map2(object[fvector_cols], argvals, 
    ~.x[, .y, matrix=FALSE])
  object
}
