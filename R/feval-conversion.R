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
  feval(data, argvals, domain = domain, range = range, ...)
}

#' @importFrom tidyr unnest
as.data.frame.feval <- function(x, rownames = NULL, optional = FALSE, 
  argvals = NULL, interpolate = FALSE, ...) {
  if (is.null(argvals)) {
    argvals <- ensure_list(argvals(x))
  } 
  argvals <- adjust_resolution(argvals, x)
  tmp <- x[, argvals, interpolate = interpolate, matrix = FALSE]
  tidyr::unnest(bind_rows(list(id = names(x), data = tmp)))
}

#' @importFrom tidyr spread
as.matrix.feval <- function(x, argvals = NULL, interpolate = FALSE) {
  df <- spread(as.data.frame(x, argvals, interpolate = interpolate), 
    key = argvals, value = data)
  ret <- as.matrix(select(df, -id))
  structure(ret, argvals = as.numeric(colnames(ret)))
}
