# conversions to/from data.frame, matrix
as.feval <- function(data, ...) UseMethod("as.feval")
as.feval.matrix  <- function(data, argvals = NULL, regular = NULL, domain = NULL, 
    range = NULL, ...) {
  feval(data, argvals, regular, domain, range, ...)
}
as.feval.data.frame <- function(data, id = 1, argvals = 2, value = 3, domain = NULL, 
    range = NULL, ...) {
  feval(data, id, argvals, value, domain, range, ...)
}
as.feval.list <- function(data, argvals = NULL, regular = NULL, domain = NULL, 
    range = NULL, ...) {
  feval(data, argvals, value, domain, range, ...)
}

as.data.frame.feval <- function(x, rownames = NULL, optional = FALSE, 
  argvals = NULL, verbose = TRUE, allow_interpolation = TRUE, ...) {
  if (is.null(argvals)) {
    argvals <- argvals(x)
    if (!is_irreg(x)) argvals <- list(argvals)
  } else {
    check_argvals(argvals, x)
    argvals <- adjust_resolution(argvals, x)
    if (verbose | !allow_interpolation) {
      interpolation <- unlist(check_interpolation(x, argvals))
      if (!allow_interpolation && any(interpolation)) {
        stop("interpolated values requested.")
        #FIXME: or just remove interpolation argvals?
      }
      if (verbose && any(interpolation)) {
        message("interpolated values requested.")
      }
    }
    if (!is.list(argvals)) argvals <- list(argvals)
  }
  tmp <- list(id = names(x), argvals = argvals,  flist = x)
  bind_rows(pmap(tmp, ~ bind_cols(id = rep(..1, length(..2)),  argvals = ..2, 
    value = do.call(..3, list(..2)))))
}

as.matrix.feval <- function(x, argvals = NULL, verbose = TRUE) {
  df <- spread(as.data.frame(x, argvals, verbose), key = argvals, value = value)
  ret <- as.matrix(select(df, -id))
  structure(ret, argvals = as.numeric(colnames(ret)))
}
