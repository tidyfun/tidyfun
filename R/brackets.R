#' Accessing/evaluating, subsetting and subassigning `fvectors`
#' 
#' These functions access, subset, replace and evaluate `fvectors`. 
#' For more information on creating `fvector`s and converting them to/from 
#' `list`, `data.frame` or `matrix`, see [feval()] and [fbase()].
#' 
#' @param x an `fvector`
#' @param i index of the observations (`integer`ish, `character` or `logical`,
#'   usual R rules apply)
#' @param j The `argvals` used to evaluate the functions. A (list of) `numeric`
#'   vectors.
#' @param interpolate should functions be evaluated (i.e., inter-/extrapolated)
#'   for `argvals` for which no original data is available? Only relevant for
#'   `feval`, defaults to TRUE.
#' @param matrix should the result be returned as a `matrix` or as a list of
#'   `data.frame`s? If TRUE, `j` has to be a (list of a) single vector of
#'   `argvals`. See return value.
#' @return If `j` is missing, a subset of the functions in `x` as given by
#'   `i`.\cr If `j` is given and `matrix == TRUE`, a numeric matrix of function
#'   evaluations in which each row represents one function and each column
#'   represents one `argval` as given in argument `j`, with an attribute
#'   `argvals`=`j` and row- and column-names derived from `x[i]` and `j`.\cr If
#'   `j` is given and `matrix == FALSE`, a list of `tbl_df`s with columns
#'   `argvals` = `j` and `data` = evaluations at `j` for each observation in
#'   `i`.
#' @import checkmate
#' @rdname fvectorbrackets
#' @export
`[.fvector` <- function(x, i, j, interpolate = TRUE, matrix = TRUE) {
  if (!interpolate & inherits(x, "fbase")) {
    interpolate <- TRUE
    message("interpolate argument ignored for data in basis representation")
  }
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
    assert_integerish(i, lower = -length(x), upper = length(x), 
      any.missing = FALSE)
    assert_true(all(sign(i) == sign(i)[1]))
  }  
  if (missing(j)) {
    ret <- unclass(x)[i]
    if (is_irreg(x)) attr(x, "argvals") <-  attr(x, "argvals")[i]
    attributes(ret) <- append(attributes(x)[names(attributes(x)) != "names"], 
      list(names = names(ret)))
    return(ret)
  } 
  if (matrix & is.list(j)) {
    stop("need a single vector-valued <j> if matrix = TRUE")
  }
  j <- adjust_resolution(ensure_list(j), x)
  evals <- evaluate(x[i], argvals = j)
  if (!interpolate) {
    new_j <- map2(j, ensure_list(argvals(x[i])), ~ !(.x %in% .y))
    if (any(unlist(new_j))) {
      warning("interpolate = FALSE & no evaluations for some <j>: NAs created.")
    }
    evals <- map2(evals, new_j, ~ ifelse(.y, NA, .x))
  }
  if (matrix) {
    ret <- do.call(rbind, evals)
    colnames(ret) <- unlist(j)
    rownames(ret) <- names(x)[i]
    structure(ret, argvals = unlist(j))
  } else {
    ret <- map2(j, evals, ~ bind_cols(argvals = .x, data = .y))
    names(ret) <- names(x)[i]
    ret
  }
} 

#' @param value `fvector` object for subassignment. This is (currently) very strictly typed,
#'  i.e. only objects that are of the same class and have compatible `argvals` can be 
#'  subassigned.
#' @rdname fvectorbrackets
#' @export
`[<-.fvector` <- function(x, i, value) {
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
    all(domain(x) == domain(value)),
    length(value) %in% c(1, length(i)))
  if (inherits(x, "feval_reg") | inherits(x, "fbase")) {
    assert_true(identical(argvals(x), argvals(value)))
  }
  if (is_feval(x)) {
    assert_true(
      identical(evaluator(x), evaluator(value), ignore.environment = TRUE))
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
  if (is_irreg(x)) {
    attr_x$argvals[i] <- argvals(value)
  }
  # fill up empty functions
  na_entries <- which(sapply(ret, is.null))
  if (length(na_entries)) {
    na_length <- if (is_feval(x)) {
      ifelse(is_irreg(x), 1, length(attr_x$argvals[[1]]))
    } else length(x[[1]])
    ret[na_entries] <- replicate(length(na_entries), rep(1*NA, na_length), 
      simplify = FALSE)
    if (is_irreg(x)) attr_x$argvals[na_entries] <- 
      replicate(length(na_entries), domain(x)[1], simplify = FALSE)
  }
  attributes(ret) <- attr_x
  ret
}
