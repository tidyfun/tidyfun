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

as.data.frame.feval <- function(x) {
  #FIXME: add argvals
  argvals <- argvals(x)
  if (inherits(x, "feval_reg")) argvals <- list(argvals)
  tmp <- list(id = names(x), argvals = argvals,  flist = x)
  bind_rows(pmap(tmp, ~ bind_cols(id = rep(..1, length(..2)),  argvals = ..2, 
    value = do.call(..3, list(..2)))))
}

as.matrix.feval_reg <- function(x, argvals = NULL) {
  #FIXME: add argvals
  argvals <- argvals(x)
  ret <- do.call(rbind,  evaluations(x))
  dimnames(ret) <- list(names(x), argvals)
  structure(ret, argvals = argvals)
}
as.matrix.feval_irreg <- function(x) {
  argvals <- argvals(x)
  grid <- sort(unique(unlist(argvals)))
  ret <- do.call(rbind, 
    pmap(list(x, argvals, list(grid)), function(.x, .a, .g) {
      tmp <- rep(NA, length(.g))
      tmp[.g %in% .a] <- evaluations(.x)
      tmp
    }))
  dimnames(ret) <- list(names(x), grid)
  structure(ret, argvals = grid)
}
