#'@import zoo
approx_fill_extend <- structure(function(f) zoo::na.fill(f, fill = "extend"), 
  label = "zoo::na.fill('extend')")
approx_linear <- structure(function(f) zoo::na.approx(f, na.rm = FALSE), 
  label = "zoo::na.approx (linear)")
approx_spline <- structure(function(f) zoo::na.spline(f, na.rm = FALSE), 
  label = "zoo::na.spline")
approx_locf <- structure(function(f) zoo::na.locf(f, na.rm = FALSE), 
  label = "zoo::na.locf")
approx_nocb <- structure(function(f) zoo::na.locf(f, na.rm = FALSE, fromLast = TRUE), 
  label = "zoo::na.locf(fromLast)")

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

#' @import checkmate
check_argvals <- function(argvals, x){
  if (is.list(argvals)) {
    c(check_list(argvals, types = "numeric", len = length(x)),
      map(argvals, ~ check_argvals_vector(x, .)))
  } else {
    check_argvals_vector(argvals, x)
  }
}
check_argvals_vector <- function(argvals, x) {
  check_numeric(argvals, any.missing = FALSE, 
    lower = domain(x)[1], upper = domain(x)[2])
}


#TODO: write proper tests for this
check_interpolation <- function(x, argvals){
  UseMethod("check_interpolation")
}
check_interpolation.feval_reg <- function(x, argvals){
  original <- argvals(x)
  if (is.list(argvals)) {
    map(argvals, ~ !(. %in% original))
  } else {
    !(argvals %in% original)
  }
}
check_interpolation.feval_irreg <- function(x, argvals) {
  original <- argvals(x)
  if (is.list(argvals)) {
    map2(argvals, original, ~ !(.x %in% .y))
  } else {
    map(original, ~ !(argvals %in% .x))
  }
}

adjust_resolution <- function(argvals, x) {
  signif <- attr(x, "signif_digits_argvals")
  if (is.list(argvals)) {
    map(argvals, ~ signif(., signif)) 
  } else {
    signif(argvals, signif)
  }  
}

make_f <- function(.argvals, .data, interpolator, signif) {
  #do this with local{}!!
  function(v) {
    v <- signif(v, signif)
    if (isTRUE(all.equal(v, .argvals))) return(.data)
    stopifnot(all(!duplicated(v)))
    v_arg <- sort(unique(c(v, .argvals)))
    v_arg_match <- match(v_arg, .argvals, nomatch = length(.argvals) + 1)
    requested <-  v_arg %in% v
    coredata(interpolator(zoo(.data[v_arg_match], v_arg)))[requested]
  }
}


#' @export
is_fvector <- function(x) "fvector" %in% class(x)
#' @export
is_irreg <- function(x) "feval_irreg" %in% class(x)
