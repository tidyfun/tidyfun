#' Evaluate `tf`s inside a `data.frame`
#'
#' @details The `arg`-argument of `tf_evaluate.data.frame` method can be a
#'   list of `arg`-vectors or -lists used as the `arg` argument for the
#'   [tf::tf_evaluate()]-method for the respective `tf`-columns in `object`.
#'   `...` is not used for a `tf`-`object`, but a second unnamed argument to these
#'   methods will be interpreted as `arg`.
#' @param object a `data.frame`-like object with `tf` columns.
#' @param ... optional: a selection of `tf`-columns. If empty, all `tf`-variables
#'   in the data frame are selected. You can supply bare variable names,
#'   select all variables between `x` and `z` with `x:z`, exclude `y` with `-y`.
#'   For more options, see the [dplyr::select()] documentation.
#' @param arg optional evaluation grid (vector or list of vectors).
#'   Defaults to `tf_arg(object)`.
#' @returns Replaces `tf`-columns with list columns of
#'   smaller `data.frames` containing the functions' arguments (`arg`) and
#'   evaluations (`value`) and returns the modified nested dataframe.
#' @export
#' @import tf
#' @importFrom tidyselect vars_select quos
#' @importFrom rlang enquos quo_text
#' @importFrom purrr map map_lgl pmap
#' @family tidyfun data wrangling functions
tf_evaluate.data.frame <- function(object, ..., arg) {
  # figure out which tf columns to evaluate:
  tf_cols <- names(object)[map_lgl(object, is_tf)]
  tf_to_evaluate <- enquos(...)
  if (!is_empty(tf_to_evaluate)) {
    tf_to_evaluate <- unname(vars_select(names(object), !!!tf_to_evaluate))
    tf_cols <- intersect(tf_cols, tf_to_evaluate)
  }
  if (!length(tf_cols)) {
    warning("Nothing to be done for tf_evaluate.", call. = FALSE)
    return(object)
  }
  if (!missing(arg) && !is.null(arg)) {
    arg <- tf::ensure_list(arg)
    if (length(arg) == 1 && length(tf_cols) > 1) {
      arg <- replicate(length(tf_cols), arg, simplify = FALSE)
    }
  } else {
    arg <- map(object[tf_cols], \(x) tf::ensure_list(tf_arg(x)))
  }
  stopifnot(length(arg) == length(tf_cols))
  names(arg) <- tf_cols
  # convert them to list-columns of data.frames
  for (f in tf_cols) {
    object[[f]] <- object[[f]][, arg[[f]], matrix = FALSE]
  }
  object
}
