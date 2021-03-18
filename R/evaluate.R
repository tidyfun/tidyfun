#' Evaluate `tf`s, both inside or outside a `data.frame`
#'
#' @details The `arg`-argument of `tf_evaluate.data.frame` method can be a
#'   list of `arg`-vectors or -lists used as the `arg` argument for the
#'   [tf_evaluate()]-method for the respective `tf`-columns in `object`.
#'   `...` is not used for a `tf`-`object`, but a second unnamed argument to these
#'   methods will be interpreted as `arg`.
#' @param object a `tf`, or a `data.frame`-like object with `tf` columns.
#' @param ... optional: a selection of `tf`-columns. If empty, all `tf`-variables
#'   in the data frame are selected. You can supply bare variable names,
#'   select all variables between `x` and `z` with `x:z`, exclude `y` with `-y`.
#'   For more options, see the [dplyr::select()] documentation. 
#' @param arg optional evaluation grid (vector or list of vectors).
#'   Defaults to `tf_arg(object)`.
#' @param evaluator optional. The function to use for inter/extrapolating the `tfd`.
#'   Defaults to `tf_evaluator(object)`. See e.g. [tf_approx_linear()] for details.
#' @return For `tf`-objects, a list of numeric vectors containing the function
#'   evaluations. For dataframes, replaces `tf`-columns with list columns of
#'   smaller `data.frames` containing the functions' arguments (`arg`) and
#'   evaluations (`value`).
#' @seealso This is used internally by `[.tf` to evaluate `object`.
#' @export
tf_evaluate <- function(object, ..., arg) UseMethod("tf_evaluate")

#' @export
tf_evaluate.default <- function(object, ..., arg) .NotYetImplemented()

#' @export
#' @rdname tf_evaluate
tf_evaluate.tfd <- function(object, ..., arg, evaluator = tf_evaluator(object)) {
  if (missing(arg)) {
    if (nargs() == 1) return(tf_evaluations(object))
    if (nargs() == 2) arg <- list(...)[[1]]
    if (nargs() > 2) {
      stop("too many arguments - can't use ...")
    }
  }
  if (is.null(arg)) return(tf_evaluations(object))
  arg <- ensure_list(arg)
  assert_arg(arg, object, check_unique = FALSE)
  ret <- pmap(
    list(arg, ensure_list(tf_arg(object)), tf_evaluations(object)),
    ~evaluate_tfd_once(
      new_arg = ..1, arg = ..2, evaluations = ..3,
      evaluator = evaluator,
      resolution = tf_resolution(object)
    )
  )
  
  names(ret) <- names(object)
  ret
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
tf_evaluate.tfb <- function(object, ..., arg) {
  if (missing(arg)) {
    if (nargs() == 1) return(tf_evaluations(object))
    if (nargs() == 2) arg <- list(...)[[1]]
    if (nargs() > 2) {
      stop("too many unnamed arguments - can't use ...")
    }
  }
  if (is.null(arg)) return(tf_evaluations(object))
  arg <- ensure_list(arg)
  assert_arg(arg, object, check_unique = FALSE)
  if (length(arg) == 1) {
    arg <- unlist(arg)
    evals <- evaluate_tfb_once(
      x = arg,
      arg = tf_arg(object),
      coefs = do.call(cbind, coef(object)),
      basis = attr(object, "basis"),
      X = attr(object, "basis_matrix"),
      resolution = tf_resolution(object)
    )
    ret <- if (length(arg) == 1) {
      # avoid cast to simple vector for point evaluations
      split(evals, seq_along(evals))
    } else {
      split(evals, col(as.matrix(evals)))
    }  
  } else {
    ret <- pmap(
      list(arg, ensure_list(tf_arg(object)), coef(object)),
      ~evaluate_tfb_once(
        x = ..1, arg = ..2, coefs = ..3,
        basis = attr(object, "basis"), X = attr(object, "basis_matrix"),
        resolution = tf_resolution(object)
      )
    )
  }
  if (!inherits(object, "tfb_fpc")) {
    ret <- map(ret, attr(object, "family")$linkinv)
  }  
  names(ret) <- names(object)
  ret
}

evaluate_tfb_once <- function(x, arg, coefs, basis, X, resolution) {
  seen <- match(
    round_resolution(x, resolution),
    round_resolution(arg, resolution)
  )
  seen_index <- na.omit(seen)
  seen <- !is.na(seen)
  if (all(seen)) return(drop(X[seen_index, , drop = FALSE] %*% coefs))
  Xnew <- X[rep(1, length(x)), ]
  if (any(seen)) Xnew[seen, ] <- X[seen_index, , drop = FALSE]
  Xnew[!seen, ] <- basis(x[!seen])
  drop(Xnew %*% coefs)
}

#' @rdname tf_evaluate
#' @import tidyr
#' @importFrom tidyselect vars_select quos
#' @importFrom rlang quo_text
#' @export
#' @examples 
#' library(dplyr)
#' a <- tf_rgp(3, arg = 7)
#' b <- tf_rgp(3, arg = 7)
#' d <- tibble(a = a, b = b)
#' tf_evaluate(a) %>% str()
#' tf_evaluate(a, arg = .5) %>% str()
#' tf_evaluate(d) %>% glimpse()
#' tf_evaluate(d, -b) %>% glimpse()
#' tf_evaluate(d, a) %>% glimpse() 
#' grid <- seq(0, 1, l = 11)
#' # <arg> must be specified as named argument!
#' tf_evaluate(d, arg = grid) %>% glimpse() 
tf_evaluate.data.frame <- function(object, ..., arg) {
  # figure out which tf columns to evaluate:
  tf_cols <- names(object)[map_lgl(object, is_tf)]
  tf_to_evaluate <- enquos(...)
  if (!is_empty(tf_to_evaluate)) {
    tf_to_evaluate <- unname(vars_select(names(object), !!!tf_to_evaluate))
    tf_cols <- intersect(tf_cols, tf_to_evaluate)
  }
  if (!length(tf_cols)) {
    warning("Nothing to be done for tf_evaluate.")
    return(object)
  }
  if (!missing(arg) && !is.null(arg)) {
    arg <- ensure_list(arg)
    if (length(arg) == 1 & length(tf_cols) > 1) {
      arg <- replicate(length(tf_cols), arg, simplify = FALSE)
    }
  } else {
    arg <- map(object[tf_cols], ~ensure_list(tf_arg(.)))
  }
  stopifnot(length(arg) == length(tf_cols))
  names(arg) <- tf_cols
  # convert them to list-columns of data.frames
  for (f in tf_cols) {
    object[[f]] <- object[[f]][, arg[[f]], matrix = FALSE]
  }
  object
}
