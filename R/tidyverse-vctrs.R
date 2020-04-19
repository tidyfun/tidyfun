c_names <- function(funs) {
  fnames <- as.list(names(funs) %||% rep(".", length(funs)))
  elnames <- map(funs, ~names(.x) %||% rep(".", length(.x)))
  # always use argnames
  # argnames replace elementnames if elments have length 1
  # else paste with "."
  names <- map2(fnames, elnames, function(.x, .y) {
    if (.x == "") return(.y)
    if (all(.y == "") | length(.y) == 1) return(rep(.x, length(.y)))
    paste(.x, .y, sep = ".")
  }) %>% unlist()
  if (all(names == "")) NULL else names
}



## Need to add defaults? proxy stuff? 

#' vctrs methods for tf objects
#' @name vctrs
#' @export
#' @export vec_ptype2.tfd
#' @inheritParams vctrs::vec_ptype2
vec_ptype2.tfd <- function(x, y, ...) UseMethod("vec_ptype2.tfd")

#' @name vctrs
#' @export vec_ptype2.tfb
#' @inheritParams vctrs::vec_ptype2
vec_ptype2.tfb <- function(x, y, ...) UseMethod("vec_ptype2.tfb")


#' @name vctrs
#' @method vec_ptype2.tfd tfd
#' @export
vec_ptype2.tfd.tfd = function(x, y, ...) {
  
  funs <- list(x, y)
  compatible <- do.call(rbind, map(
    funs,
    ~compare_tf_attribs(funs[[1]], .)
  ))
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
    warning(
      "inputs have different resolutions, result has ",
      "resolution =", new_resolution
    )
    make_irreg[!compatible[, "resolution"]] <- TRUE
  }
  if (any(make_irreg)) {
    funs <- map_at(
      funs, which(make_irreg),
      ~as.tfd_irreg(.)
    )
  }
  if (!all(compatible[, "evaluator_name"])) {
    warning(
      "inputs have different evaluators, result has ",
      attr(funs[[1]], "evaluator_name")
    )
  }
  attr_ret <- attributes(funs[[1]])
  if (any(irreg | make_irreg)) {
    attr_ret$arg <- flatten(map(funs, tf_arg))
  }
  attr_ret$names <- {
    tmp <- unlist(flatten(map(
      funs,
      function(x) names(x) %||% rep("", length(x))
    )))
    if (all(tmp == "")) NULL else tmp
  }
  ret <- flatten(funs)
  attributes(ret) <- attr_ret
  names(ret) <- c_names(funs)
  forget(attr(ret, "evaluator"))
  ret
}


#' @name vctrs
#' @method vec_ptype2.tfb tfb
#' @export
vec_ptype2.tfb.tfb = function(x, y, ...) {

  funs <- list(x, y)
  compatible <- do.call(rbind, map(
    funs,
    ~compare_tf_attribs(funs[[1]], .)
  ))
  stopifnot(all(compatible[, "domain"]))
  re_evals <- which(!compatible[, "arg"] |
                      !compatible[, "basis_args"])
  if (length(re_evals)) {
    fun_names <- map(as.list(match.call())[-1], ~deparse(.)[1])
    warning(
      "re-evaluating ", paste(fun_names[re_evals], collapse = ", "),
      " using basis and arg of ", fun_names[1]
    )
    funs <- map_at(
      funs, re_evals,
      ~do.call(
        tfb,
        flatten(list(list(.),
                     arg = list(tf_arg(funs[[1]])),
                     attr(funs[[1]], "basis_args")
        ))
      )
    )
  }
  if (!all(compatible[, "resolution"])) {
    warning(
      "inputs have different resolutions, result has ",
      "resolution =", attr(funs[[1]], "resolution")
    )
  }
  attr_ret <- attributes(funs[[1]])
  attr_ret$names <- {
    tmp <- unlist(flatten(map(
      funs,
      function(x) names(x) %||% rep("", length(x))
    )))
    if (all(tmp == "")) NULL else tmp
  }
  ret <- flatten(funs)
  attributes(ret) <- attr_ret
  names(ret) <- c_names(funs)
  ret
}




# minimal vec_cast implementation: https://github.com/r-spatial/sf/issues/1068
#' @name vctrs
#' @export
#' @inheritParams vctrs::vec_cast
#' @param x_arg,y_arg Argument names for \code{x} and \code{y}.
vec_cast.tfd <- function(x, to, ...) UseMethod("vec_cast.tfd")

#' @name vctrs
#' @export
#' @inheritParams vctrs::vec_cast
#' @param x_arg,y_arg Argument names for \code{x} and \code{y}.
vec_cast.tfb <- function(x, to, ...) UseMethod("vec_cast.tfb")


#' @name vctrs
#' @export
vec_cast.tfd.tfd <- function(x, to, ...) x


#' @name vctrs
#' @export
vec_cast.tfb.tfb <- function(x, to, ...) x

#' @name vctrs
#' @export
vec_cast.tfb.tfd <- function(x, to, ...) stop("Can't coerce tfb & tfd objects.")

#' @name vctrs
#' @export
vec_cast.tfd.tfb <- function(x, to, ...) stop("Can't concatenate tfb & tfd objects.")

