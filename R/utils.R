ensure_list <- function(x) if (!is.list(x)) list(x) else x

unique_id <- function(x)  {
  if (!any(duplicated(x))) return(x)
  if (is.character(x))  x <- sub("$^", "?", x)
  x <- make.unique(make.names(as.character(x)))
  #TODO: make sure this has the correct order (here or in converters?)
  x
} 

#'@import zoo
zoo_wrapper <- function(f, ...){
  dots <- list(...)
  function(x, arg, evaluations) {
      x_arg <- sort(unique(c(x, arg)))
      x_arg_match <- match(x_arg, arg, nomatch = length(arg) + 1)
      requested <-  x_arg %in% x
      dots[[length(dots) + 1]] <- zoo(evaluations[x_arg_match], x_arg)
      ret <- do.call(f, dots)
      coredata(ret)[requested]
  }
}
approx_linear <- zoo_wrapper(na.approx, na.rm = FALSE)
approx_spline <- zoo_wrapper(na.spline, na.rm = FALSE)
approx_fill_extend <- zoo_wrapper(na.fill, fill = "extend")
approx_locf <- zoo_wrapper(na.locf, na.rm = FALSE)
approx_nocb <- zoo_wrapper(na.locf, na.rm = FALSE, fromLast = TRUE)

in_range <- function(x, r){
  r <- range(r, na.rm = TRUE)
  x >= r[1] & x <= r[2]
}
na_to_0 <- function(x) {
  x[is.na(x)] <- 0
  x
}

find_arg <- function(data, arg) {
  if (is.null(arg)) {
    suppressWarnings(arg <- as.numeric(dimnames(data)[[2]]))
    if (is.null(arg) | any(is.na(arg))) {
      message("Column names of <data> not suitable as arg.")
      arg <- numeric(0)
    }
  } 
  if (!length(arg)) arg <- seq_len(dim(data)[2])
  stopifnot(length(arg)  == dim(data)[2], 
    is.numeric(arg), all(!is.na(arg)))
  list(arg)
}

#' @import checkmate
assert_arg <- function(arg, x){
  if (is.list(arg)) {
    assert_true(length(arg) %in% c(1, length(x)))
    map(arg, ~ assert_arg_vector(., x = x))
  } else {
    assert_arg_vector(arg, x)
  }
}
assert_arg_vector <- function(arg, x) {
  assert_numeric(arg, any.missing = FALSE, unique = TRUE,
    lower = domain(x)[1], upper = domain(x)[2])
}



# #TODO: write proper tests for this
# check_interpolation <- function(x, arg){
#   UseMethod("check_interpolation")
# }
# check_interpolation.tfd_reg <- function(x, arg){
#   original <- arg(x)
#   if (is.list(arg)) {
#     map(arg, ~ !(. %in% original))
#   } else {
#     !(arg %in% original)
#   }
# }
# check_interpolation.tfd_irreg <- function(x, arg) {
#   original <- arg(x)
#   if (is.list(arg)) {
#     map2(arg, original, ~ !(.x %in% .y))
#   } else {
#     map(original, ~ !(arg %in% .x))
#   }
# }

adjust_resolution <- function(arg, f) {
  signif <- attr(f, "signif_arg")
  .adjust_resolution(arg, signif)
}

.adjust_resolution <- function(arg, signif, unique = TRUE){
  u <- if (unique) base::unique else function(x) x
  if (is.list(arg)) {
    map(arg, ~ u(signif(., signif)))
  } else {
    u(signif(arg, signif))
  }  
}

compare_tf_attribs <- function(e1, e2, ignore = "names") {
# TODO: better way to check evaluator/basis functions?
  a1 <- attributes(e1)
  a2 <- attributes(e2)
  attribs <- union(names(a1), names(a2))
  if (length(ignore)) attribs <- attribs[!(attribs %in% ignore)]
  .compare <- function(a, b) {
    if (is.null(a) != is.null(b)) return(FALSE)
    suppressWarnings(
      if (is.function(a)) {
        #FIXME: this is not reliable/useful but prob. impossible to solve
        #generally: would need to know which (functional) objects in the enclosure
        #of these functions are relevant for comparison -- comparing all is too
        #strict but comparing none is rather dangerous. Right now the function
        #bodies all look the same since they share a common wrapper.... Fingers
        #crossed relevant differences get picked up by differences in the label or
        #basis attributes...
        if (is.memoised(a)) {
          identical(environment(a)[["_f"]], environment(b)[["_f"]], 
            ignore.environment = TRUE)
        } else identical(a, b, ignore.environment = TRUE)
      } else {
        if (is.list(a)) {
          all(unlist(map2(a, ensure_list(b), .compare)))
        } else isTRUE(all.equal(a, b))
      })
  }  
  ret <- map(attribs, ~.compare(a1[[.]], a2[[.]]))
  names(ret) <- attribs
  unlist(ret)
}

#-------------------------------------------------------------------------------

#' @rdname tfmethods
#' @export
is_tf <- function(x) "tf" %in% class(x)

#' @rdname tfmethods
#' @export
is_irreg <- function(x) "tfd_irreg" %in% class(x)

#' @rdname tfmethods
#' @export
is_tfd <- function(x) "tfd" %in% class(x)

#' @rdname tfmethods
#' @export
is_tfb <- function(x) "tfb" %in% class(x)

#-------------------------------------------------------------------------------

#' @rdname tfmethods
is.na.tf <- function(x) {
  map_lgl(unclass(x), ~ any(is.na(.x)))
}
