#' Evaluate `tf`s, both inside or outside a `data.frame`
#' 
#' The `evaluate.data.frame` method evaluates `tf`-columns inside a `data.frame`
#' into list columns of smaller `data.frames` containing the functions' arguments 
#' (`arg`) and evaluations (`value`). Its `arg`-argument can be a list of `arg`-vectors 
#' used as the `arg` argument for the [evaluate()]-method for the respective
#' `tf`-columns in `object`.
#' @param object an `tf` or a `data.frame`-like object with `tf` columns
#' @param arg optional evaluation grid, defaults to `arg(object)`. 
#' @seealso \code{?`[.tf`}
#' @export
evaluate <- function(object, arg, ...) UseMethod("evaluate")

#' @export
evaluate.default <- function(object, arg, ...) .NotYetImplemented()

#' @export
#' @rdname evaluate
evaluate.tfd <- function(object, arg, ...) {
  if (missing(arg) | is.null(arg)) arg <- tidyfun::arg(object)
  arg <- ensure_list(arg)
  assert_arg(arg, object)
  pmap(list(arg, ensure_list(arg(object)), evaluations(object)), 
    ~ evaluate_tfd_once(x = ..1, arg = ..2, evaluations = ..3, 
        evaluator = attr(object, "evaluator")))
}  

evaluate_tfd_once <- function(x, arg, evaluations, evaluator) {
  if (isTRUE(all.equal(x, arg))) return(evaluations)
  evaluator(x, arg = arg, evaluations = evaluations)
}

#' @export
#' @rdname evaluate
evaluate.tfb <- function(object, arg, ...) {
  if (missing(arg) | is.null(arg)) arg <- tidyfun::arg(object)
  arg <- ensure_list(arg)
  assert_arg(arg, object)
  if (length(arg) == 1) {
    arg <- unlist(arg)
    evals <- evaluate_tfb_once(x = arg, 
      arg = attr(object, "arg"), 
      coefs = do.call(cbind, coef(object)),
      basis = attr(object, "basis"),
      X = attr(object, "basis_matrix"))
    ret <- split(evals, col(evals))
  } else {
    ret <- pmap(list(arg, ensure_list(arg(object)), coef(object)),
      ~ evaluate_tfb_once(x = ..1, arg = ..2, coefs = ..3, 
        basis = attr(object, "basis"), X = attr(object, "basis_matrix")))
  }
  names(ret) <- names(object)
  ret
}  

evaluate_tfb_once <- function(x, arg, coefs, basis, X) {
  dejavu <- match(x, arg)
  dejavu_index <- na.omit(dejavu)
  dejavu <- !is.na(dejavu)
  if (all(dejavu)) return(X[dejavu_index, ,drop = FALSE] %*% coefs)
  Xnew <- X[rep(1, length(x)),]
  if (any(dejavu)) Xnew[dejavu,] <- X[dejavu_index, , drop = FALSE]
  Xnew[!dejavu,] <- basis(x[!dejavu])
  Xnew %*% coefs
}


#' @rdname evaluate
#' @param ... optional:  A selection of columns. If empty, all `tfd`-variables 
#'   are selected. You can supply bare variable names, 
#'   select all variables between `x` and `z` with `x:z`, exclude `y` with `-y`. 
#'   For more options, see the [dplyr::select()] documentation.
#' @import tidyr
#' @importFrom tidyselect vars_select quos
#' @importFrom rlang quo_text
#' @export
evaluate.data.frame <- function(object, arg, ...) {
  quos <- quos(...)
  # figure out which tf columns to evaluate
  tf_cols <- names(object)[map_lgl(object, is_tf)]
  if (!is_empty(quos)) {
    to_eval <- unname(vars_select(names(object), !!!quos))
    tf_cols <- intersect(tf_cols, to_eval)
  }
  if (!length(tf_cols)) {
    return(object)
  }
  if (!missing(arg)) {
    arg <- ensure_list(arg)
    if (length(arg) == 1 & length(tf_cols) > 1) {
      arg <- replicate(length(tf_cols), arg, simplify = FALSE)
    }  
  } else {
    arg <- map(object[tf_cols], ~ ensure_list(tidyfun::arg(.)))
  }
  stopifnot(length(arg) == length(tf_cols))
  names(arg) <- tf_cols
  # convert them to list-columns of data.frames
  for (f in tf_cols) {
    object[[f]] <- object[[f]][, arg[[f]], matrix = FALSE]
  }
  object
}
