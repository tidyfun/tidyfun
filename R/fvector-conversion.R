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

as.data.frame.feval <- function(x, rownames = NULL, optional = FALSE, 
  argvals = NULL, interpolate = FALSE, ...) {
  if (is.null(argvals)) {
    argvals <- ensure_list(argvals(x))
  } 
  argvals <- adjust_resolution(argvals, x)
  tmp <- x[, argvals, interpolate = interpolate, matrix = FALSE]
  tidyr::unnest(bind_rows(list(id = names(x), data = tmp)))
}


as.matrix.feval <- function(x, argvals = NULL, interpolate = FALSE) {
  ret <- as.data.frame(x, argvals = argvals, interpolate = interpolate)  %>% 
    arrange(argvals) %>% 
    tidyr::spread(key = argvals, value = data) %>% 
    select(-id) %>% 
    as.matrix
  structure(ret, argvals = as.numeric(colnames(ret)))
}

#-------------------------------------------------------------------------------

# conversions to/from data.frame, matrix
as.fbase <- function(data, ...) UseMethod("as.fbase")

as.fbase.matrix  <- function(data, argvals = NULL, basis = 'ps', 
  domain = NULL, range = NULL, penalized = FALSE, signif = 4, ...) {
  fbase(data, basis = basis, domain = domain, range = range, 
    penalized = penalized, signif = signif, ...)
}
as.fbase.data.frame <- function(data, id = 1, argvals = 2, value = 3, basis = 'ps', 
  domain = NULL, range = NULL, penalized = FALSE, signif = 4, ...) {
  fbase(data, id, argvals, value, basis = basis, domain = domain, range = range,
    penalized = penalized, signif = signif, ...)
}
as.fbase.list <- function(data, argvals = NULL, basis = 'ps', 
  domain = NULL, range = NULL, penalized = FALSE, signif = 4, ...) {
  fbase(data, basis = basis, domain = domain, range = range, 
    penalized = penalized, signif = signif, ...)
}

as.data.frame.fbase <- function(x, rownames = NULL, optional = FALSE, 
  argvals = NULL, ...) {
  if (is.null(argvals)) {
    argvals <- ensure_list(argvals(x))
  } 
  argvals <- adjust_resolution(argvals, x)
  tmp <- x[, argvals, matrix = FALSE]
  tidyr::unnest(bind_rows(list(id = names(x), data = tmp)))
}


as.matrix.fbase <- function(x, argvals = NULL) {
  ret <- as.data.frame(x, argvals = argvals)  %>% 
    arrange(argvals) %>% 
    tidyr::spread(key = argvals, value = data) %>% 
    select(-id) %>% 
    as.matrix
  structure(ret, argvals = as.numeric(colnames(ret)))
}
