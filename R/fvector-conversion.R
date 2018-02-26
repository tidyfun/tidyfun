#' @rdname feval
#' @export 
as.feval <- function(data, ...) UseMethod("as.feval")

#' @rdname feval
#' @export 
as.feval.matrix  <- function(data, argvals = NULL, domain = NULL, 
      ...) {
  feval(data, argvals = argvals, domain = domain,   ...)
}

#' @rdname feval
#' @export 
as.feval.data.frame <- function(data, id = 1, argvals = 2, value = 3, domain = NULL, 
      ...) {
  feval(data, id = id, argvals = argvals, value = value, domain = domain,   ...)
}

#' @rdname feval
#' @export 
as.feval.list <- function(data, argvals = NULL, domain = NULL, ...) {
  feval(data, argvals = argvals, domain = domain,   ...)
}

#' @rdname feval
#' @export 
as.feval.fbase <- function(data, argvals = NULL, domain = NULL, ...) {
  feval(data, argvals = argvals, domain = domain,   ...)
}

#' @rdname feval
#' @param rownames not used
#' @param optional not used
#' @param x an `feval` object
#' @inheritParams [.fvector
#' @export 
as.data.frame.feval <- function(x, rownames = NULL, optional = FALSE, 
  argvals = NULL, interpolate = FALSE, ...) {
  if (is.null(argvals)) {
    argvals <- ensure_list(argvals(x))
  } 
  argvals <- adjust_resolution(argvals, x)
  tmp <- x[, argvals, interpolate = interpolate, matrix = FALSE]
  tidyr::unnest(bind_rows(list(id = names(x), data = tmp)))
}

#' @rdname feval
#' @export
as.matrix.feval <- function(x, argvals = NULL, interpolate = FALSE, ...) {
  ret <- as.data.frame(x, argvals = argvals, interpolate = interpolate)  %>% 
    arrange(argvals) %>% 
    tidyr::spread(key = argvals, value = data) %>% 
    select(-id) %>% 
    as.matrix
  structure(ret, argvals = as.numeric(colnames(ret)))
}

#-------------------------------------------------------------------------------

#' @rdname fbase
#' @export 
as.fbase <- function(data, ...) UseMethod("as.fbase")

#' @rdname fbase
#' @export 
as.fbase.matrix  <- function(data, argvals = NULL, 
  domain = NULL,   penalized = TRUE, signif = 4, ...) {
  fbase(data,  domain = domain,   
    penalized = penalized, signif = signif, ...)
}

#' @rdname fbase
#' @export 
as.fbase.data.frame <- function(data, id = 1, argvals = 2, value = 3, 
  domain = NULL,   penalized = TRUE, signif = 4, ...) {
  fbase(data, id, argvals, value, domain = domain,  
    penalized = penalized, signif = signif, ...)
}

#' @rdname fbase
#' @export 
as.fbase.list <- function(data, argvals = NULL, 
  domain = NULL,   penalized = TRUE, signif = 4, ...) {
  fbase(data, argvals, domain = domain,   
    penalized = penalized, signif = signif, ...)
}

#' @rdname fbase
#' @export 
as.fbase.feval <- function(data, argvals = NULL, 
  domain = NULL,   penalized = TRUE, signif = 4, ...) {
  fbase(data, argvals, domain = domain,   
    penalized = penalized, signif = signif, ...)
}

#' @rdname fbase
#' @param rownames not used
#' @param optional not used
#' @param x an `fbase` object
#' @export 
as.data.frame.fbase <- function(x, rownames = NULL, optional = FALSE, 
  argvals = NULL, ...) {
  if (is.null(argvals)) {
    argvals <- ensure_list(argvals(x))
  } 
  argvals <- adjust_resolution(argvals, x)
  tmp <- x[, argvals, matrix = FALSE]
  tidyr::unnest(bind_rows(list(id = names(x), data = tmp)))
}

#' @rdname fbase
as.matrix.fbase <- function(x, argvals = NULL, ...) {
  ret <- as.data.frame(x, argvals = argvals)  %>% 
    arrange(argvals) %>% 
    tidyr::spread(key = argvals, value = data) %>% 
    select(-id) %>% 
    as.matrix
  structure(ret, argvals = as.numeric(colnames(ret)))
}
