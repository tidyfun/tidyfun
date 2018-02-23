# new generics & methods 

#' Utility functions for `fvector`-objects
#' 
#' A bunch of methods that do what they say.
#' @param f an `fvector` object
#' 
#' @rdname fvectormethods
#' @export
argvals <- function(f) UseMethod("argvals")
#' @export
argvals.default <- function(f) .NotYetImplemented()
#' @export
argvals.feval_irreg <- function(f) attr(f, "argvals")
#' @export
argvals.feval_reg <- function(f) attr(f, "argvals")[[1]]
#' @export
argvals.fbase <- function(f) attr(f, "argvals")

#' @rdname fvectormethods
#' @export
evaluations <- function(f) UseMethod("evaluations")
#' @export
evaluations.default <- function(f) .NotYetImplemented()
#' @export
evaluations.feval <- function(f) {
  attributes(f) <- NULL
  f
}  
#' @export
evaluations.fbase <- function(f) {
  map(f, ~ drop(attr(f, "basis_matrix") %*% .))
} 


#' @rdname fvectormethods
#' @export
n_evaluations <- function(f) UseMethod("n_evaluations")
#' @export
n_evaluations.default <- function(f) .NotYetImplemented()
#' @export
n_evaluations.feval_irreg <- function(f) map_int(evaluations(f), length)
#' @export
n_evaluations.feval_reg <- function(f) length(argvals(f))

#' @rdname fvectormethods
#' @export
domain <- function(f) {
  stopifnot(inherits(f, "fvector"))
  attr(f, "domain")
}
  
#' @export
evaluator <- function(f) {
  stopifnot(inherits(f, "feval"))
  attr(f, "evaluator")
}

#' @export
basis <- function(f) {
  stopifnot(inherits(f, "fbase"))
  attr(f, "basis")
}


#' @rdname fvectormethods
#' @param value a function that can be used to interpolate an `feval`. Needs to
#'   accept vector arguments `x`, `argvals`, `evaluations` and return
#'   evaluations of the function defined by `argvals`, `evaluations` at `x`
#' @export
`evaluator<-` <- function(x, value) {
  stopifnot(inherits(x, "feval"), is.function(value))
  attr(x, "evaluator_name") <- deparse(value, width.cutoff = 60)[1]
  attr(x, "evaluator") <- memoise(eval(value))
  x
}

#-------------------------------------------------------------------------------
# new methods
#' @rdname fvectormethods
#' @param ... dots
#' @param na.rm as usual
#' @export
range.fvector <- function(..., na.rm = FALSE) {
  d <- list(...) 
  stopifnot(length(d) == 1)
  range(unlist(evaluations(d[[1]])))
}

#' @export
coef.fbase <- function(object, ...) {
  attributes(object) <- NULL
  object
}


#' Pretty printing and formatting for functional data
#' 
#' Print/format `fvector`-objects.
#' 
#' @rdname fvectordisplay
#' @param n how many elements of `x` to print out
print.fvector <- function(x, n  = 10, ...) {
  cat(paste0("fvector[",length(x),"] on (", domain(x)[1], ",",
    domain(x)[2], ")"))
  invisible(x)
}

#' @rdname fvectordisplay
#' @export
print.feval_reg <- function(x, n = 10, ...) {
  NextMethod()
  cat(" based on", length(argvals(x)), "evaluations each\n")
  cat("interpolation by", attr(x, "evaluator_name"), "\n")
  cat(format(x[1:min(n, length(x))], ...), sep = "\n")
  if (n < length(x)) 
    cat(paste0("    [....]   (", length(x) - n, " not shown)\n"))
  invisible(x)
}

#' @rdname fvectordisplay
#' @export
print.feval_irreg <- function(x, n = 10, ...) {
  NextMethod()
  n_evals <- n_evaluations(x[!is.na(names(x))])
  cat(paste0(" based on ", min(n_evals), " to ", max(n_evals)," (mean: ",
    round(mean(n_evals)),") evaluations each\n"))
  cat("inter-/extrapolation by", attr(x, "evaluator_name"), "\n")
  cat(format(x[1:min(n, length(x))], ...), sep = "\n")
  if (n < length(x)) 
    cat(paste0("    [....]   (", length(x) - n, " not shown)\n"))
  invisible(x)
}

#' @rdname fvectordisplay
#' @export
print.fbase <- function(x, n = 10, ...) {
  NextMethod()
  cat(" in basis representation:\n basis call: ", attr(x, "basis_label"), "\n")
  cat(format(x[1:min(n, length(x))], ...), sep = "\n")
  if (n < length(x)) 
    cat(paste0("    [....]   (", length(x) - n, " not shown)\n"))
  invisible(x)
}

# FIXME: this needs proper width align etc arguments like format.default
#' @rdname fvectordisplay
#' @inheritParams base::format.default
#' @export
format.fvector <- function(x, digits = 2, nsmall = 0, ...){
   argvals <- ensure_list(attr(x, "argvals"))
   str <- map2_chr(argvals, evaluations(x), string_rep_fvector, 
     signif_argvals = attr(x, "signif_argvals"), 
     digits = digits, nsmall = nsmall, ... = ...)
   map2_chr(names(x)[1:length(str)], str, ~ paste0(.x,": ",.y))
}



#summary #define Arith-methods first.... 
# c.feval_reg #???

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
    new_j <- map2(j, ensure_list(argvals(x)), ~ !(.x %in% .y))
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


# plot
# deriv
# mean
# quantile
# var
# sd
# cov
# cor
# max
# min

### new generics:
# integrate

