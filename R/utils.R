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


make_f <- function(.argvals, .data, interpolator) {
  function(v) {
    stopifnot(all(!duplicated(v)))
    v_arg <- sort(unique(c(v, .argvals)))
    v_arg_match <- match(v_arg, .argvals, nomatch = length(.argvals) + 1)
    requested <-  v_arg %in% v
    coredata(interpolator(zoo(.data[v_arg_match], v_arg)))[requested]
  }
}

#'@import zoo
approx_fill_extend <- structure(function(f) zoo:na.fill(f, fill = "extend"), 
  label = "zoo::na.fill('extend')")
approx_linear <- structure(function(f) zoo:na.approx(f, na.rm = FALSE), 
  label = "zoo:na.approx")
approx_spline <- structure(function(f) zoo:na.spline(f, na.rm = FALSE), 
  label = "zoo:na.spline")
approx_locf <- structure(function(f) zoo:na.locf(f, na.rm = FALSE), 
  label = "zoo:na.locf")
approx_nocb <- structure(function(f) zoo:na.locf(f, na.rm = FALSE, fromLast = TRUE), 
    label = "zoo:na.locf(fromLast)")

