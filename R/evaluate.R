#' Evaluate `tf`s, both inside or outside a `data.frame`
#' 
#' The `evaluate.data.frame` method evaluates `tf`-columns inside a `data.frame`
#' into list columns of smaller `data.frames` containing the functions' arguments 
#' (`arg`) and evaluations (`value`). Its `arg`-argument can be a list of `arg`-vectors 
#' used as the `arg` argument for the [tf_evaluate()]-method for the respective
#' `tf`-columns in `object`.
#' @param object an `tf` or a `data.frame`-like object with `tf` columns
#' @param arg optional evaluation grid, defaults to `arg(object)`. 
#' @seealso \code{?`[.tf`}
#' @export
tf_evaluate <- function(object, arg, ...) UseMethod("tf_evaluate")

#' @export
tf_evaluate.default <- function(object, arg, ...) .NotYetImplemented()

#' @export
#' @rdname tf_evaluate
tf_evaluate.tfd <- function(object, arg, ...) {
  if (missing(arg)) arg <- tidyfun::arg(object)
  if (is.null(arg)) arg <- tidyfun::arg(object)
  arg <- ensure_list(arg)
  assert_arg(arg, object, check_unique = FALSE)
  pmap(list(arg, ensure_list(arg(object)), tf_evaluations(object)), 
    ~ evaluate_tfd_once(new_arg = ..1, arg = ..2, evaluations = ..3, 
        evaluator = attr(object, "evaluator"), 
        resolution = resolution(object)))
}  

evaluate_tfd_once <- function(new_arg, arg, evaluations, evaluator, resolution) {
  new_arg_round <- round_resolution(new_arg, resolution)
  arg_round <- round_resolution(arg, resolution)
  if (isTRUE(all.equal(new_arg_round, arg_round))) return(evaluations)
  seen <- match(new_arg_round, arg_round)
  seen_index <- na.omit(seen)
  seen <- !is.na(seen)
  ret <- rep(NA, length(new_arg))
  ret[seen] <- evaluations[seen_index]
  ret[!seen] <- 
    evaluator(new_arg[!seen], arg = arg, evaluations = evaluations)
  ret
}

#' @export
#' @rdname tf_evaluate
tf_evaluate.tfb <- function(object, arg, ...) {
  if (missing(arg)) arg <- tidyfun::arg(object)
  if (is.null(arg)) arg <- tidyfun::arg(object)
  arg <- ensure_list(arg)
  assert_arg(arg, object, check_unique = FALSE)
  if (length(arg) == 1) {
    arg <- unlist(arg)
    evals <- evaluate_tfb_once(x = arg, 
      arg = arg(object), 
      coefs = do.call(cbind, coef(object)),
      basis = attr(object, "basis"),
      X = attr(object, "basis_matrix"),
      resolution = resolution(object))
    ret <- split(evals, col(evals))
  } else {
    ret <- pmap(list(arg, ensure_list(arg(object)), coef(object)),
      ~ evaluate_tfb_once(x = ..1, arg = ..2, coefs = ..3, 
        basis = attr(object, "basis"), X = attr(object, "basis_matrix"),
        resolution = resolution(object)))
  }
  names(ret) <- names(object)
  ret
}  

evaluate_tfb_once <- function(x, arg, coefs, basis, X, resolution) {
  seen <- match(round_resolution(x, resolution), 
    round_resolution(arg, resolution))
  seen_index <- na.omit(seen)
  seen <- !is.na(seen)
  if (all(seen)) return(X[seen_index, ,drop = FALSE] %*% coefs)
  Xnew <- X[rep(1, length(x)),]
  if (any(seen)) Xnew[seen,] <- X[seen_index, , drop = FALSE]
  Xnew[!seen,] <- basis(x[!seen])
  Xnew %*% coefs
}


#' @rdname tf_evaluate
#' @param ... optional:  A selection of columns. If empty, all `tfd`-variables 
#'   are selected. You can supply bare variable names, 
#'   select all variables between `x` and `z` with `x:z`, exclude `y` with `-y`. 
#'   For more options, see the [dplyr::select()] documentation.
#' @import tidyr
#' @importFrom tidyselect vars_select quos
#' @importFrom rlang quo_text
#' @export
tf_evaluate.data.frame <- function(object, arg, ...) {
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
  if (!missing(arg) && !is.null(arg)) {
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
