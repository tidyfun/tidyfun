#' @rdname feval
#' @param ... arguments for [feval()]
#' @export 
as.feval <- function(data, ...) UseMethod("as.feval")
as.feval.default <- function(data, ...) {
  feval(data,  ...)
}

# TODO: this ignores argvals, domain for now, only needed internally in c.feval
#' @rdname feval
as.feval_irreg <- function(data, signif = NULL, ...) UseMethod("as.feval_irreg")

as.feval_irreg.feval_reg <- function(data, signif = NULL, ...) {
  class(data)[1] <- "feval_irreg"
  argvals <- attr(data, "argvals")
  if (!is.null(signif)) {
    argvals <- .adjust_resolution(argvals, signif = signif, unique = TRUE)
    if (length(argvals[[1]]) != length(attr(data, "argvals")[[1]])) {
      stop("Can't convert data to lower resolution: creates non-unique argvals.")
    }
  }
  attr(data, "argvals") <- replicate(length(data), argvals)
  data
}
# TODO: this ignores argvals, domain for now.....
as.feval_irreg.feval_irreg <- function(data, signif = 4, ...) {
  argvals <- attr(data, "argvals")
  if (!is.null(signif)) {
    argvals <- .adjust_resolution(argvals, signif = signif, unique = TRUE)
    if (any(rapply(argvals, length) != rapply(attr(data, "argvals"), length))) {
      stop("Can't convert data to lower resolution: creates non-unique argvals.")
    }
  }
  attr(data, "argvals") <- argvals
  data
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
#' @param basis either "mgcv" to call [fbase()] which uses `mgcv`-type spline basis functions
#'   or "fpc" to call [fpcbase()] which uses a (smoothed) functional principal component basis. 
#' @param ... arguments to [fbase()] or [fpcbase()]
#' @export 
as.fbase <- function(data, basis = c("mgcv", "fpc"), ...) UseMethod("as.fbase")
as.fbase.default <- function(data, basis = c("mgcv", "fpc"), ...) {
  basis <- match.arg(basis)
  fbase_maker <- switch(basis, "mgcv" = fbase, "fpc" = fpcbase)
  fbase_maker(data, ...)
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
