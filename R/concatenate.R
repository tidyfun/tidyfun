
#-------------------------------------------------------------------------------

#' Concatenate `fvector`-objects
#' 
#' Functions to concatenate multiple vectors of functional data.
#' 
#' Currently only allows concatenation of very similar functions (same class, same domain).\cr
#' `c.fbase` will use the basis of its first argument for representing all remaining arguments
#'    and refit them accordingly if necessary.\cr
#' `c.feval` will use the `evaluator` of its first argument for all remaining arguments as well.
#' If `feval_reg`-objects to be concatenated are not on the same grid of `argvals`, they will 
#' be evaluated on the `argvals` of the first argument. 
#' 
#' This means that `c(f1, f2) == rev(c(f2, f1))` is not necessarily all true!
#' 
#' @param ... for `c()`: a bunch of `fvector`-objects on the same domain and of the same class. Not used for `merge`.
#' @return an `fvector`-object containing all the arguments, of the same class and with the same attributes as the
#'   the first argument.
#' @rdname fvectorconcat
#' @export
c.fvector <- function(...) {
  funs <- list(...)
  classes <- unique(map_chr(funs, ~class(.)[1]))
  if (length(classes) != 1) {
    # TODO: should allow casting?
    stop("Can't concatenate ", paste(classes, collapse = " & "), " objects.")
  }
}

#' @rdname fvectorconcat
#' @export
c.feval <- function(...) {
  funs <- list(...)
  if (length(funs) == 1) {
    return(funs[[1]])
  } else NextMethod()  
  irreg <- is_irreg(funs[[1]])
  compatible <- do.call(rbind, map(funs, 
    ~ compare_fvector_attribs(funs[[1]], .)))
  stopifnot(all(compatible[, "domain"]))
  if (!irreg & !all(compatible[, "argvals"])) {
    fun_names <- map(as.list(match.call())[-1], deparse)
    re_evals <- fun_names[which(!compatible[, "argvals"])]
    warning("re-evaluating arguments ", 
      paste(re_evals, collapse = ", "), 
      " on argvals of first argument.")
    modify_at(funs, which(!compatible[, "argvals"]), 
      ~feval(., argvals = argvals(funs[[1]])))
    # TODO: or just return an feval_irreg object?
  }    
  if (!all(compatible[, "evaluator_name"])) {
    warning("inputs have different evaluators, result has ", 
      attr(funs[[1]], "evaluator_name"))
  }
  if (!all(compatible[, "signif_argvals"])) {
    warning("inputs have different resolutions, result has ", 
      "signif_argvals =", attr(funs[[1]], "signif_argvals"))
  }
  attr_ret <- attributes(funs[[1]])
  if (irreg) {
    attr_ret$argvals <- flatten(map(funs, argvals))
  }
  attr_ret$names <- make.unique(unlist(flatten(map(funs, names))))
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
