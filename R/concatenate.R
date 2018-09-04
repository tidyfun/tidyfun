c_names <- function(funs) {
  fnames <- as.list(names(funs) %||% rep("", length(funs)))
  elnames <- map(funs, ~ names(.x) %||% rep("", length(.x)))
  # always use argnames 
  # argnames replace elementnames if elments have length 1
  # else paste with "."
  names <- map2(fnames, elnames, function(.x, .y) {
    if (.x == "") return(.y)
    if (all(.y == "") | length(.y) == 1) return(rep(.x, length(.y)))
    paste(.x, .y, sep = ".")  
  })  %>% unlist
  if (all(names == "")) NULL else names
}

#' Concatenate `tf`-objects
#' 
#' Functions to concatenate multiple vectors of functional data.
#' 
#' Only allows concatenation of functions with the same domain and similar
#' represenation, i.e., `tfb` cannot be concatenated to `tfd` and vice
#' versa. If `tfd_reg`-objects to be concatenated are not on the same grid of
#' `arg`, or if both `tfd_reg` and `tfd_irreg` objects are concatenated,
#' a `tfd_irreg`-object is returned. \cr `c.tfb` will use the basis of its
#' first argument for representing all remaining arguments and refit them
#' accordingly if necessary.\cr `c.tfd` will use the `evaluator` of its first
#' argument for all remaining arguments as well.\cr This means that `c(f1, f2)`
#' is not necessarily the same as `rev(c(f2, f1))`.
#' 
#' @param ... for `c()`: a bunch of `tf`-objects on the same domain and of
#'   the same class. Not used for `merge`.
#' @return an `tf`-object containing all the arguments with the same
#'   attributes as the the first argument (see Details).
#' @rdname tfconcat
c.tf <- function(...) {
  funs <- list(...)
  compatible <- all(map_lgl(funs, is_tfd)) | all(map_lgl(funs, is_tfb))
  if (!compatible) {
    stop("Can't concatenate tfb & tfd objects.")
  }
}

#' @rdname tfconcat
#' @export
c.tfd <- function(...) {
  funs <- list(...)
  if (length(funs) == 1) {
    return(funs[[1]])
  } else NextMethod()  
  compatible <- do.call(rbind, map(funs, 
    ~ compare_tf_attribs(funs[[1]], .)))
  stopifnot(all(compatible[, "domain"]))
  make_irreg <- rep(FALSE, length(funs))
  irreg <- map_lgl(funs, is_irreg)
  if (!any(irreg) & !all(compatible[, "arg"])) {
    warning("concatenating functions on different grids.")
    make_irreg <- rep(TRUE, length(funs))
  }
  if (any(irreg) & !all(irreg)) {
    warning("concatenating functions on different grids.")
    make_irreg[!irreg] <- TRUE
  }
  new_resolution <- NULL
  if (!all(compatible[, "resolution"])) {
    new_resolution <- tf_resolution(funs[[1]])
    warning("inputs have different resolutions, result has ", 
      "resolution =", new_resolution)
    make_irreg[!compatible[, "resolution"]] <- TRUE
  }
  if (any(make_irreg)) {
    funs <- map_at(funs, which(make_irreg), 
      ~ as.tfd_irreg(.))
  }
  if (!all(compatible[, "evaluator_name"])) {
    warning("inputs have different evaluators, result has ", 
      attr(funs[[1]], "evaluator_name"))
  }
  attr_ret <- attributes(funs[[1]])
  if (any(irreg | make_irreg)) {
    attr_ret$arg <- flatten(map(funs, tf_arg))
  }
  attr_ret$names <- {
    tmp <- unlist(flatten(map(funs, 
      function(x) names(x) %||% rep("", length(x)))))
    if (all(tmp == "")) NULL else tmp
  }
  ret <- flatten(funs)
  attributes(ret) <- attr_ret
  names(ret) <- c_names(funs)
  forget(attr(ret, "evaluator"))
  ret
}

#' @rdname tfconcat
#' @export
c.tfb <- function(...) {
  funs <- list(...)
  if (length(funs) == 1) {
    return(funs[[1]])
  } else NextMethod()  
  compatible <- do.call(rbind, map(funs, 
    ~ compare_tf_attribs(funs[[1]], .)))
  stopifnot(all(compatible[, "domain"]))
  re_evals <- which(!compatible[, "arg"] | 
      !compatible[, "basis_args"])
  if (length(re_evals)) {
    fun_names <- map(as.list(match.call())[-1], ~deparse(.)[1])
    warning("re-evaluating ", paste(fun_names[re_evals], collapse = ", "), 
      " using basis and arg of ", fun_names[1])
    funs <- map_at(funs,re_evals, 
      ~do.call(tfb, 
        flatten(list(list(.), arg = list(tf_arg(funs[[1]])), 
          attr(funs[[1]], "basis_args")))))
  }    
  if (!all(compatible[, "resolution"])) {
    warning("inputs have different resolutions, result has ", 
      "resolution =", attr(funs[[1]], "resolution"))
  }
  attr_ret <- attributes(funs[[1]])
  attr_ret$names <- {
    tmp <- unlist(flatten(map(funs, 
      function(x) names(x) %||% rep("", length(x)))))
    if (all(tmp == "")) NULL else tmp
  }  
  ret <- flatten(funs)
  attributes(ret) <- attr_ret
  names(ret) <- c_names(funs)
  ret
}

#' @param x `tf`-object 
#' @param y `tf`-object
#' @rdname tfconcat
#' @export
merge.tf <- function(x, y, ...) {
  c(x, y)
}
