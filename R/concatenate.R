
#-------------------------------------------------------------------------------

#' Concatenate `fvector`-objects
#' 
#' Functions to concatenate multiple vectors of functional data.
#' 
#' Only allows concatenation of functions with the same domain and similar represenation,
#' i.e., `fbase` cannot be concatenated to `feval` and vice versa. 
#' If `feval_reg`-objects to be concatenated are not on the same grid of `argvals`, or if both
#' `feval_reg` and `feval_irreg` objects are concatenated, a `feval_irreg`-object is returned.
#' \cr
#' `c.fbase` will use the basis of its first argument for representing all remaining arguments
#'    and refit them accordingly if necessary.\cr
#' `c.feval` will use the `evaluator` of its first argument for all remaining arguments as well.\cr
#' This means that `c(f1, f2)` is not necessarily the same as `rev(c(f2, f1))`.
#' 
#' @param ... for `c()`: a bunch of `fvector`-objects on the same domain and of the same class. Not used for `merge`.
#' @return an `fvector`-object containing all the arguments with the same attributes as the
#'   the first argument (see Details).
#' @rdname fvectorconcat
c.fvector <- function(...) {
  funs <- list(...)
  compatible <- all(map_lgl(funs, is_feval)) | all(map_lgl(funs, is_fbase))
  if (!compatible) {
    stop("Can't concatenate fbase & feval objects.")
  }
}

#' @rdname fvectorconcat
#' @export
c.feval <- function(...) {
  funs <- list(...)
  if (length(funs) == 1) {
    return(funs[[1]])
  } else NextMethod()  
  compatible <- do.call(rbind, map(funs, 
    ~ compare_fvector_attribs(funs[[1]], .)))
  stopifnot(all(compatible[, "domain"]))
  make_irreg <- rep(FALSE, length(funs))
  irreg <- map_lgl(funs, is_irreg)
  if (!any(irreg) & !all(compatible[, "argvals"])) {
    warning("concatenating functions on different grids.")
    make_irreg <- rep(TRUE, length(funs))
  }
  if (any(irreg) & !all(irreg)) {
    warning("concatenating functions on different grids.")
    make_irreg[!irreg] <- TRUE
  }
  new_signif <- NULL
  if (!all(compatible[, "signif_argvals"])) {
    new_signif <- attr(funs[[1]], "signif_argvals")
    warning("inputs have different resolutions, result has ", 
      "signif_argvals =", new_signif)
    make_irreg[!compatible[, "signif_argvals"]] <- TRUE
  }
  if (any(make_irreg)) {
    funs <- map_at(funs, which(make_irreg), 
      ~ as.feval_irreg(., signif = new_signif))
  }
  if (!all(compatible[, "evaluator_name"])) {
    warning("inputs have different evaluators, result has ", 
      attr(funs[[1]], "evaluator_name"))
  }
  attr_ret <- attributes(funs[[1]])
  if (any(irreg | make_irreg)) {
    attr_ret$argvals <- flatten(map(funs, argvals))
  }
  attr_ret$names <- unique_id(unlist(flatten(map(funs, names))))
  ret <- flatten(funs)
  attributes(ret) <- attr_ret
  forget(attr(ret, "evaluator"))
  ret
}

#' @rdname fvectorconcat
#' @export
c.fbase <- function(...) {
  funs <- list(...)
  if (length(funs) == 1) {
    return(funs[[1]])
  } else NextMethod()  
  compatible <- do.call(rbind, map(funs, 
    ~ compare_fvector_attribs(funs[[1]], .)))
  stopifnot(all(compatible[, "domain"]))
  re_evals <- which(!compatible[, "argvals"] | 
      !compatible[, "basis_args"])
  if (length(re_evals)) {
    fun_names <- map(as.list(match.call())[-1], ~deparse(.)[1])
    warning("re-evaluating ", paste(fun_names[re_evals], collapse = ", "), 
      " using basis and argvals of ", fun_names[1])
    funs <- map_at(funs,re_evals, 
      ~do.call(fbase, 
        flatten(list(list(.), argvals = list(argvals(funs[[1]])), 
          attr(funs[[1]], "basis_args")))))
  }    
  if (!all(compatible[, "signif_argvals"])) {
    warning("inputs have different resolutions, result has ", 
      "signif_argvals =", attr(funs[[1]], "signif_argvals"))
  }
  attr_ret <- attributes(funs[[1]])
  attr_ret$names <- make.unique(unlist(flatten(map(funs, names))))
  ret <- flatten(funs)
  attributes(ret) <- attr_ret
  ret
}
#' @param x `fvector`-object 
#' @param y `fvector`-object
#' @rdname fvectorconcat
#' @export
merge.fvector <- function(x, y, ....) {
  c(x, y)
}
