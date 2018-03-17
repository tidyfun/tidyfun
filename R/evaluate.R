#' Evaluate `fvector`s, both inside or outside a `data.frame`
#' 
#' 
#' The `evaluate.data.frame` method evaluates `fvector`-columns inside a `data.frame`
#' into list columns of smaller `data.frames` containing the functions' argvals and 
#' evaluations. Its `argvals`-argument can be a list of `argvals`-vector 
#' used as the `argvals` argument for the [evaluate()]-method for the respective
#' `fvector`-columns in `object`.
#' @param object an `fvector` or a `data.frame`-like object with `fvector` columns
#' @param argvals optional evaluation grid, defaults to `argvals(object)`. 
#' @seealso \code{?`[.fvector`}
#' @export
evaluate <- function(object, argvals, ...) UseMethod("evaluate")

#' @export
evaluate.default <- function(object, argvals, ...) .NotYetImplemented()

#' @export
#' @rdname evaluate
evaluate.feval <- function(object, argvals, ...) {
  if (missing(argvals) | is.null(argvals)) argvals <- tidyfun::argvals(object)
  argvals <- ensure_list(argvals)
  assert_argvals(argvals, object)
  pmap(list(argvals, ensure_list(argvals(object)), evaluations(object)), 
    ~ evaluate_feval_once(x = ..1, argvals = ..2, evaluations = ..3, 
        evaluator = attr(object, "evaluator")))
}  

evaluate_feval_once <- function(x, argvals, evaluations, evaluator) {
  if (isTRUE(all.equal(x, argvals))) return(evaluations)
  evaluator(x, argvals = argvals, evaluations = evaluations)
}

#' @export
#' @rdname evaluate
evaluate.fbase <- function(object, argvals, ...) {
  if (missing(argvals) | is.null(argvals)) argvals <- tidyfun::argvals(object)
  argvals <- ensure_list(argvals)
  assert_argvals(argvals, object)
  if (length(argvals) == 1) {
    argvals <- unlist(argvals)
    evals <- evaluate_fbase_once(x = argvals, 
      argvals = attr(object, "argvals"), 
      coefs = do.call(cbind, coef(object)),
      basis = attr(object, "basis"),
      X = attr(object, "basis_matrix"))
    ret <- split(evals, col(evals))
  } else {
    ret <- pmap(list(argvals, ensure_list(argvals(object)), coef(object)),
      ~ evaluate_fbase_once(x = ..1, argvals = ..2, coefs = ..3, 
        basis = attr(object, "basis"), X = attr(object, "basis_matrix")))
  }
  names(ret) <- names(object)
  ret
}  

evaluate_fbase_once <- function(x, argvals, coefs, basis, X) {
  dejavu <- match(x, argvals)
  dejavu_index <- na.omit(dejavu)
  dejavu <- !is.na(dejavu)
  if (all(dejavu)) return(X[dejavu_index, ,drop = FALSE] %*% coefs)
  Xnew <- X[rep(1, length(x)),]
  if (any(dejavu)) Xnew[dejavu,] <- X[dejavu_index, , drop = FALSE]
  Xnew[!dejavu,] <- basis(x[!dejavu])
  Xnew %*% coefs
}


#' @rdname evaluate
#' @param ... optional: names of the `fvector`-columns to unnest 
#' @import tidyr
#' @importFrom tidyselect vars_select quos
#' @importFrom rlang quo_text
#' @export
#' @md
evaluate.data.frame <- function(object, argvals, ...) {
  quos <- quos(...)
  # figure out which fvector columns to evaluate
  fvector_cols <- names(object)[map_lgl(object, is_fvector)]
  if (!is_empty(quos)) {
    fvector_cols <- intersect(fvector_cols, map_chr(quos, rlang::quo_text))
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
    ~.x[, .y, matrix = FALSE])
  object
}
