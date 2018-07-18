#' @rdname tfd
#' @export 
as.tfd <- function(data, ...) UseMethod("as.tfd")
as.tfd.default <- function(data, ...) {
  tfd(data,  ...)
}

# TODO: this ignores argvals, domain for now, only needed internally in c.tfd
#' @rdname tfd
as.tfd_irreg <- function(data, signif = NULL, ...) UseMethod("as.tfd_irreg")

as.tfd_irreg.tfd_reg <- function(data, signif = NULL, ...) {
  argvals <- ensure_list(argvals(data))
  if (!is.null(signif)) {
    argvals <- .adjust_resolution(argvals, signif = signif, unique = TRUE)
    if (length(argvals[[1]]) != length(argvals(data))) {
      stop("Converting 'data' to lower resolution would create non-unique argvals.")
    }
  }
  ret <- map2(evaluations(data), argvals, ~ list(argvals = .y, data = .x))
  attributes(ret) <- attributes(data)
  attr(ret, "argvals") <- numeric(0)
  class(ret)[1] <- "tfd_irreg"
  ret
}

# TODO: this ignores argvals, domain for now.....
as.tfd_irreg.tfd_irreg <- function(data, signif = 4, ...) {
  argvals <- argvals(data)
  if (!is.null(signif)) {
    argvals <- .adjust_resolution(argvals, signif = signif, unique = TRUE)
    if (any(rapply(argvals, length) != rapply(attr(data, "argvals"), length))) {
      stop("Can't convert data to lower resolution: creates non-unique argvals.")
    }
  }
  argvals(data) <- argvals
  data
}

#' @rdname tfd
#' @param row.names not used
#' @param optional not used
#' @param x an `tfd` object
#' @inheritParams [.tf
#' @export 
as.data.frame.tfd <- function(x, row.names = NULL, optional = FALSE, 
  argvals = NULL, interpolate = FALSE, ...) {
  if (is.null(argvals)) {
    argvals <- ensure_list(argvals(x))
  } 
  argvals <- adjust_resolution(argvals, x)
  tmp <- x[, argvals, interpolate = interpolate, matrix = FALSE]
  id <- unique_id(names(x)) %||% seq_along(x)
  id <- ordered(id, levels = id) # don't reshuffle 
  tidyr::unnest(bind_rows(list(id = id, data = tmp))) 
}

#' @rdname tfd
#' @export
as.matrix.tfd <- function(x, argvals = NULL, interpolate = FALSE, ...) {
  if (is.null(argvals)) {
    argvals <- sort(unlist(argvals(x)))
  } 
  argvals <- adjust_resolution(argvals, x)
  ret <- x[, argvals, interpolate = interpolate, matrix = TRUE]
  structure(ret, argvals = as.numeric(colnames(ret)))
}

#-------------------------------------------------------------------------------

#' @rdname tfb
#' @param basis either "mgcv" to call [tfb()] which uses `mgcv`-type spline basis functions
#'   or "fpc" to call [fpcbase()] which uses a (smoothed) functional principal component basis. 
#' @param ... arguments to [tfb()] or [fpcbase()]
#' @export 
as.tfb <- function(data, basis = c("mgcv", "fpc"), ...) UseMethod("as.tfb")
as.tfb.default <- function(data, basis = c("mgcv", "fpc"), ...) {
  basis <- match.arg(basis)
  tfb_maker <- switch(basis, "mgcv" = tfb, "fpc" = fpcbase)
  tfb_maker(data, ...)
}

#' @rdname tfb
#' @param row.names not used
#' @param optional not used
#' @param x an `tfb` object
#' @export 
as.data.frame.tfb <- function(x, row.names = NULL, optional = FALSE, 
  argvals = NULL, ...) {
  if (is.null(argvals)) {
    argvals <- ensure_list(argvals(x))
  } 
  argvals <- adjust_resolution(argvals, x)
  tmp <- x[, argvals, matrix = FALSE]
  id <- unique_id(names(x)) %||% seq_along(x)
  id <- ordered(id, levels = id) # don't reshuffle 
  tidyr::unnest(bind_rows(list(id = id, data = tmp)))
}

#' @rdname tfb
as.matrix.tfb <- function(x, argvals = NULL, ...) {
  ret <- as.data.frame(x, argvals = argvals)  %>% 
    arrange(argvals) %>% 
    tidyr::spread(key = argvals, value = data) %>% 
    select(-id) %>% 
    as.matrix
  rownames(ret) <- names(x)
  structure(ret, argvals = as.numeric(colnames(ret)))
}

#-------------------------------------------------------------------------------
#' @export 
as.function.tf <- function(x, ...) {
  function(argvals) unlist(evaluate(object = x, argvals = argvals))
}
