#' Accessing, evaluating, subsetting and subassigning `tf` vectors
#' 
#' These functions access, subset, replace and evaluate `tf` objects. 
#' For more information on creating `tf` objects and converting them to/from 
#' `list`, `data.frame` or `matrix`, see [tfd()] and [tfb()]. \cr
#' Note that these break certain (terrible) R conventions for vector-like objects:\cr
#'  
#' - no argument recycling,
#' - no indexing with `NA`,
#' - no indexing with names not present in `x`,
#' - no indexing with integers `> length(x)`
#' 
#' All of these will trigger errors. Subassigning new elements to positions
#' beyond the original length still works and will fill up the missing elements
#' up to that position with `NAs`, though. This package was developed by fickle,
#' rainbow-colored unicorns.
#' 
#' 
#' @param x an `tf`
#' @param i index of the observations (`integer`ish, `character` or `logical`,
#'   usual R rules apply)
#' @param j The `arg` used to evaluate the functions. A (list of) `numeric`
#'   vectors.
#' @param interpolate should functions be evaluated (i.e., inter-/extrapolated)
#'   for values in `arg` for which no original data is available? Only relevant for
#'   `tfd`, defaults to `TRUE`.
#' @param matrix should the result be returned as a `matrix` or as a list of
#'   `data.frame`s? If `TRUE`, `j` has to be a (list of a) single vector of
#'   `arg`. See return value.
#' @return If `j` is missing, a subset of the functions in `x` as given by
#'   `i`.\cr If `j` is given and `matrix == TRUE`, a numeric matrix of function
#'   evaluations in which each row represents one function and each column
#'   represents one `argval` as given in argument `j`, with an attribute
#'   `arg`=`j` and row- and column-names derived from `x[i]` and `j`.\cr If
#'   `j` is given and `matrix == FALSE`, a list of `tbl_df`s with columns
#'   `arg` = `j` and `value` = evaluations at `j` for each observation in
#'   `i`.
#' @import checkmate
#' @rdname tfbrackets
#' @export
#' @aliases index.tf
`[.tf` <- function(x, i, j, interpolate = TRUE, matrix = TRUE) {
  if (!interpolate & inherits(x, "tfb")) {
    interpolate <- TRUE
    message("interpolate argument ignored for data in basis representation")
  }
  if (!missing(i)) {
    assert_atomic(i)
    if (is.logical(i)) {
      assert_logical(i, any.missing = FALSE, len = length(x))
      i <- which(i)
    }
    if (is.character(i)) {
      assert_subset(i, names(x))
      i <- match(i, names(x))
    }
    assert_integerish(i, lower = -length(x), upper = length(x), 
      any.missing = FALSE)
    assert_true(all(sign(i) == sign(i)[1]))
    attr_x <- append(attributes(unname(x)), list(names = names(x)[i]))
    x <- unclass(x)[i]
    attributes(x) <- attr_x
  } else {
    i <- seq_along(x)
  }
  if (missing(j)) {
    return(x)
  } 
  if (matrix & is.list(j)) {
    stop("need a single vector-valued <j> if matrix = TRUE")
  }
  j <- ensure_list(j)
  if (!(length(j) %in% c(1, length(i)))) {
    stop("wrong length for <j>")
  } 
  evals <- evaluate(x, arg = j)
  if (!interpolate) {
    new_j <- map2(j, ensure_list(arg(x)), ~ !(.x %in% .y))
    if (any(unlist(new_j))) {
      warning("interpolate = FALSE & no evaluations for some <j>: NAs created.")
    }
    evals <- map2(evals, new_j, ~ ifelse(.y, NA, .x))
  }
  if (matrix) {
    ret <- do.call(rbind, evals)
    colnames(ret) <- unlist(j)
    rownames(ret) <- names(x)
    structure(ret, arg = unlist(j))
  } else {
    ret <- map2(j, evals, ~ bind_cols(arg = .x, value = .y))
    names(ret) <- names(x)
    ret
  }
} 

#' @param value `tf` object for subassignment. This is (currently) very strictly typed,
#'  i.e. only objects that are of the same class and have compatible `arg` can be 
#'  subassigned.
#' @rdname tfbrackets
#' @export
`[<-.tf` <- function(x, i, value) {
  if (missing(i)) {
    i <- seq_along(x)
  } else {
    assert_atomic(i)
    if (is.logical(i)) {
      assert_logical(i, any.missing = FALSE, len = length(x))
      i <- which(i)
    }
    if (is.character(i)) {
      assert_subset(i, names(x))
      i <- match(i, names(x))
    }
    assert_integerish(i, lower = -length(x), 
      any.missing = FALSE)
    assert_true(all(sign(i) == sign(i)[1]))
    if (sign(i)[1] < 0) {
      i <- (1:length(x))[i]
    }
  }
  stopifnot(inherits(value, class(x)[1]), 
    all(tf_domain(x) == tf_domain(value)),
    length(value) %in% c(1, length(i)))
  if (inherits(x, "tfd_reg") | inherits(x, "tfb")) {
    assert_true(identical(arg(x), arg(value)))
  }
  if (is_tfd(x)) {
    assert_true(
      identical(tf_evaluator(x), tf_evaluator(value), ignore.environment = TRUE))
  } else {
    assert_true(
      identical(basis(x), basis(value), ignore.environment = TRUE))
    assert_true(
      all.equal(attr(x, "basis_matrix"), attr(value, "basis_matrix")))
  }
  
  attr_x <- attributes(x)
  attr_x$names[i] <- names(value)
  ret <- unclass(x)
  ret[i] <- unclass(value)
  # fill up empty functions
  na_entries <- which(sapply(ret, is.null))
  if (length(na_entries)) {
    nas <- if (is_irreg(x)) {
      replicate(length(na_entries), list(arg = attr_x$domain[1], value = NA),
        simplify = FALSE) 
    } else {
      replicate(length(na_entries), rep(NA, length(x[[1]])), simplify = FALSE) 
    }  
    ret[na_entries] <- nas
  }
  attributes(ret) <- attr_x
  if (!is.null(names(ret))) names(ret)[is.na(names(ret))] <- ""
  ret
}
