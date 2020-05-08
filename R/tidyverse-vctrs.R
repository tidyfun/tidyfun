c_names <- function(funs) {
  fnames <- as.list(names(funs) %||% rep("", length(funs)))
  elnames <- map(funs, ~names(.x) %||% rep("", length(.x)))
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


#----------------- s3 generics for tfd casting -----------------#

#' vctrs methods for \code{tf} objects
#' 
#' These functions are the extensions that allow \code{tidyfun} vectors 
#' to work with \code{vctrs}.
#' 
#' @rdname vctrs
#' @import vctrs
#' @method vec_cast tfd_reg
#' @export
#' @export vec_cast.tfd_reg
#' @inheritParams vctrs::vec_cast
vec_cast.tfd_reg <- function(x, to, ...) UseMethod("vec_cast.tfd_reg")

#' @rdname vctrs
#' @method vec_cast tfd_irreg
#' @export
#' @export vec_cast.tfd_irreg
vec_cast.tfd_irreg <- function(x, to, ...) UseMethod("vec_cast.tfd_irreg")


#' @rdname vctrs
#' @method vec_cast.tfd_reg tfd_reg
#' @export
vec_cast.tfd_reg.tfd_reg <- function(x, to, ...) { x } 

#' @rdname vctrs
#' @method vec_cast.tfd_reg tfd_irreg
#' @export
vec_cast.tfd_reg.tfd_irreg <- function(x, to, ...) { stop("casting irreg to reg is not allowed") }

#' @rdname vctrs
#' @method vec_cast.tfd_irreg tfd_reg
#' @export
vec_cast.tfd_irreg.tfd_reg <- function(x, to, ...) { 
  
  args = attr(x, "arg")
  cast_x = tfd(map(.x = vctrs::vec_data(x), ~data.frame(arg = args, value = .x)))
  as.tfd_irreg.tfd_reg(cast_x)
  
}

#' @rdname vctrs
#' @method vec_cast.tfd_irreg tfd_irreg
#' @export
vec_cast.tfd_irreg.tfd_irreg <- function(x, to, ...) { x }



#----------------- s3 generics for tfd coercion -----------------#

#' coercion methods for tfd objects
#' @name vctrs
#' @method vec_ptype2 tfd_reg
#' @export
#' @export vec_ptype2.tfd_reg
#' @inheritParams vctrs::vec_ptype2
vec_ptype2.tfd_reg <- function(x, y, ...) UseMethod("vec_ptype2.tfd_reg")

#' @name vctrs
#' @method vec_ptype2.tfd_reg tfd_reg 
#' @export
vec_ptype2.tfd_reg.tfd_reg <- function(x, y, ...) { vec_ptype2_tfd_tfd(x, y, ...) }

#' @name vctrs
#' @method vec_ptype2.tfd_reg tfd_irreg
#' @export
vec_ptype2.tfd_reg.tfd_irreg <- function(x, y, ...) { vec_ptype2_tfd_tfd(x, y, ...) }

#' @name vctrs
#' @method vec_ptype2 tfd_irreg
#' @export
#' @export vec_ptype2.tfd_irreg
#' @inheritParams vctrs::vec_ptype2
vec_ptype2.tfd_irreg <- function(x, y, ...) UseMethod("vec_ptype2.tfd_reg")

#' @name vctrs
#' @method vec_ptype2.tfd_irreg tfd_reg
#' @export
vec_ptype2.tfd_irreg.tfd_reg <- function(x, y, ...) {vec_ptype2_tfd_tfd(x, y, ...)}

#' @name vctrs
#' @method vec_ptype2.tfd_irreg tfd_irreg
#' @export
vec_ptype2.tfd_irreg.tfd_irreg <- function(x, y, ...) {vec_ptype2_tfd_tfd(x, y, ...)}


#----------------- main function for coercion of tfd -----------------#

#' @name vctrs
#' @export
vec_ptype2_tfd_tfd = function(x, y, ...) {
  
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


#----------------- s3 generics for tfb casting -----------------#

#' vctrs methods for \code{tf} objects
#' 
#' These functions are the extensions that allow \code{tidyfun} vectors 
#' to work with \code{vctrs}.
#' 
#' @rdname vctrs
#' @import vctrs
#' @method vec_cast tfb_spline
#' @export
#' @export vec_cast.tfb_spline
#' @inheritParams vctrs::vec_cast
vec_cast.tfb_spline <- function(x, to, ...) UseMethod("vec_cast.tfb_spline")

#' @rdname vctrs
#' @method vec_cast tfb_fpc
#' @export
#' @export vec_cast.tfd_irreg
vec_cast.tfb_fpc <- function(x, to, ...) UseMethod("vec_cast.tfb_fpc")


#' @rdname vctrs
#' @method vec_cast.tfb_spline tfb_spline
#' @export
vec_cast.tfb_spline.tfb_spline <- function(x, to, ...) { 
  attributes_to = flatten(list(list(x), 
                        arg = list(tf_arg(to)),
                        attr(to, "basis_args")))
  do.call(tfb, attributes_to)

} 

#' @rdname vctrs
#' @method vec_cast.tfb_spline tfb_fpc
#' @export
vec_cast.tfb_spline.tfb_fpc <- function(x, to, ...) { stop("casting tfb_fpc to tfb_spline is not allowed") }

#' @rdname vctrs
#' @method vec_cast.tfb_fpc tfb_spline
#' @export
vec_cast.tfb_fpc.tfb_spline <- function(x, to, ...) { stop("casting tfb_spline to tfb_fpc is not allowed")}

#' @rdname vctrs
#' @method vec_cast.tfd_irreg tfd_irreg
#' @export
vec_cast.tfb_fpc.tfb_fpc <- function(x, to, ...) { x }


#----------------- s3 generics for tfb coercion -----------------#

#' @name vctrs
#' @method vec_ptype2 tfb_spline
#' @export
#' @export vec_ptype2.tfb_spline
#' @inheritParams vctrs::vec_ptype2
vec_ptype2.tfb_spline <- function(x, y, ...) UseMethod("vec_ptype2.tfb_spline")

#' @name vctrs
#' @method vec_ptype2.tfb_spline tfb_spline
#' @export
vec_ptype2.tfb_spline.tfb_spline <- function(x, y, ...) {vec_ptype2_tfb_tfb(x, y, ...)}

#' @name vctrs
#' @method vec_ptype2.tfb_spline tfb_fpc
#' @export
vec_ptype2.tfb_spline.tfb_fpc <- function(x, y, ...) stop("concatenating tfb_spline & tfb_fpc objects is not allowed")

#' @name vctrs
#' @method vec_ptype2 tfb_fpc
#' @export
#' @export vec_ptype2.tfb_fpc
#' @inheritParams vctrs::vec_ptype2
vec_ptype2.tfb_fpc <- function(x, y, ...) UseMethod("vec_ptype2.tfb_fpc")

#' @name vctrs
#' @method vec_ptype2.tfb_fpc tfb_spline
#' @export
vec_ptype2.tfb_fpc.tfb_spline <- function(x, y, ...) stop("concatenating tfb_spline & tfb_fpc objects is not allowed")


#' @name vctrs
#' @method vec_ptype2.tfb_fpc tfb_fpc
#' @export
vec_ptype2.tfb_fpc.tfb_fpc <- function(x, y, ...) {vec_ptype2_tfb_tfb(x, y, ...)}



#----------------- main function for coercion of tfb -----------------#

#' @name vctrs
#' @export
vec_ptype2_tfb_tfb = function(x, y, ...) {
  funs <- list(x, y)
  compatible <- do.call(rbind, map(
    funs,
    ~compare_tf_attribs(funs[[1]], .)
  ))
  stopifnot(all(compatible[, "domain"]))
  
  if(inherits(funs[[1]], "tfb_spline")) {
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
          flatten(list(list(.), # converts to tfb then back to tfd
                       arg = list(tf_arg(funs[[1]])),
                       attr(funs[[1]], "basis_args")
          ))
        )
      )
    }
  }else{
    re_evals <- which(!compatible[, "arg"] |
                        !compatible[, "basis_matrix"])
    
    if (length(re_evals)){
      stop("concatenation not yet implemented for tfb_fpc vectors with different bases")
    }
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
  forget(attr(ret, "basis"))
  
  ret
  
}

