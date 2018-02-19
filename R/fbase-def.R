#' @import mgcv
smooth_spec_wrapper <- function(spec) {
  function(argvals) {
    mgcv::PredictMat(object = spec, data = data.frame(argvals = argvals))
  }
} 

fbase <- function(data, ...) UseMethod("fbase")


new_fbase <- function(argvals, evaluations, basis = "ps", domain = NULL, range = NULL, 
    penalized = FALSE, signif = 4, ...) {
  argvals <- .adjust_resolution(argvals, signif)
  argvals_u <- mgcv::uniquecombs(unlist(argvals))
  s_call <- as.call(list(quote(s), quote(argvals), bs = basis, ...))
  spec_object <- smooth.construct(eval(s_call), 
    data = data.frame(argvals = argvals_u$x), knots = NULL)
  regular <- length(argvals) == 1
  if (!penalized) {
    n_evaluations <- map(argvals, length)
    underdet <- unlist(n_evaluations) < spec_object$bs.dim
    if (any(underdet)) {
      warning("More basis functions than evaluations.", 
        " Interpolation will be unreliable.")
    }
    #regular inputs --> use qr once
    if (regular) {
      coef_list <- qr.coef(qr = qr(spec_object$X), 
        y = do.call(cbind, ensure_list(evaluations)))
      coef_list <- split(coef_list, col(coef_list))
    } else {
      id <- unlist(map2(1:length(argvals), map(argvals, length), 
        ~ rep(.x, each = .y)))
      index_list <- split(attr(argvals_u, "index"), id)
      coef_list <- map2(index_list, evaluations, 
        ~ qr.coef(qr=qr(spec_object$X[.x,]), y = .y))
    }
    # need to remove NAs if dim(basis) > length(argvals)
    coef_list[underdet] <- map(coef_list[underdet], na_to_0) 
  } else stop("Not implemented yet.")
  
  #TODO: check reconstruction error & warn !!

  domain <- domain %||% range(argvals)
  range <- range %||% range(evaluations, na.rm = TRUE)
  names(coef_list) <- names(evaluations) %||% seq_along(coef_list)
  basis_constructor <- smooth_spec_wrapper(spec_object)
  structure(coef_list, 
    domain = domain,
    range = range,
    basis = memoise(basis_constructor),
    basis_label = deparse(s_call, width.cutoff = 60)[1],
    basis_matrix = structure(spec_object$X, argvals = argvals_u),
    signif_argvals = signif, 
    class = c("fbase", "fvector"))
}


