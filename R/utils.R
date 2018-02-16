ensure_list <- function(x) if (!is.list(x)) list(x) else x


#'@import zoo
zoo_wrapper <- function(f, ...){
  dots <- list(...)
  function(x, argvals, evaluations) {
      x_arg <- sort(unique(c(x, argvals)))
      x_arg_match <- match(x_arg, argvals, nomatch = length(argvals) + 1)
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

find_argvals <- function(data, argvals) {
  if (is.null(argvals)) {
    suppressWarnings(argvals <- as.numeric(dimnames(data)[[2]]))
    if (is.null(argvals) | any(is.na(argvals))) {
      message("Column names of <data> not suitable as argvals.")
      argvals <- numeric(0)
    }
  } 
  if (!length(argvals)) argvals <- seq_len(dim(data)[2])
  stopifnot(length(argvals)  == dim(data)[2], 
    is.numeric(argvals), all(!is.na(argvals)))
  list(argvals)
}

# check_argvals <- function(argvals, x){
#   if (is.list(argvals)) {
#     check_choice(length(argvals), c(1, length(x))) #return if !TRUE
#     map(argvals, ~ check_argvals_vector(., x = x))
#   } else {
#     check_argvals_vector(argvals, x)
#   }
# }
# check_argvals_vector <- function(argvals, x) {
#   check_numeric(argvals, any.missing = FALSE, unique = TRUE,
#     lower = domain(x)[1], upper = domain(x)[2])
# }

#' @import checkmate
assert_argvals <- function(argvals, x){
  if (is.list(argvals)) {
    assert_true(length(argvals) %in% c(1, length(x)))
    map(argvals, ~ assert_argvals_vector(., x = x))
  } else {
    assert_argvals_vector(argvals, x)
  }
}
assert_argvals_vector <- function(argvals, x) {
  assert_numeric(argvals, any.missing = FALSE, unique = TRUE,
    lower = domain(x)[1], upper = domain(x)[2])
}



# #TODO: write proper tests for this
# check_interpolation <- function(x, argvals){
#   UseMethod("check_interpolation")
# }
# check_interpolation.feval_reg <- function(x, argvals){
#   original <- argvals(x)
#   if (is.list(argvals)) {
#     map(argvals, ~ !(. %in% original))
#   } else {
#     !(argvals %in% original)
#   }
# }
# check_interpolation.feval_irreg <- function(x, argvals) {
#   original <- argvals(x)
#   if (is.list(argvals)) {
#     map2(argvals, original, ~ !(.x %in% .y))
#   } else {
#     map(original, ~ !(argvals %in% .x))
#   }
# }

adjust_resolution <- function(argvals, x) {
  signif <- attr(x, "signif_argvals")
  if (is.list(argvals)) {
    map(argvals, ~ signif(., signif)) 
  } else {
    signif(argvals, signif)
  }  
}

#' @export
is_fvector <- function(x) "fvector" %in% class(x)
#' @export
is_irreg <- function(x) "feval_irreg" %in% class(x)
