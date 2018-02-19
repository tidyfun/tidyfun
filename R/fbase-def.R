#' @import mgcv
smooth_spec_wrapper <- function(spec) {
  function(argvals) {
    mgcv::Predict.matrix(object = spec, data = data.frame(argvals = argvals))
  }
} 

fbase <- function(data, ...) UseMethod("fbase")

new_fbase <- function(data, regular, basis = "ps", domain = NULL, range = NULL, 
    penalized = FALSE, signif = 4, ...) {
  browser()
  data$argvals <- .adjust_resolution(data$argvals, signif)
  argvals_u <- mgcv::uniquecombs(data$argvals, ordered = TRUE)
  s_args <- list(...)[names(list(...)) %in% names(formals(mgcv::s))]
  gam_args <- list(...)[names(list(...)) %in% names(formals(mgcv::gam))]
  s_call <- as.call(c(quote(s), quote(argvals), flatten(list(bs = basis, s_args))))
  spec_object <- smooth.construct(eval(s_call), 
    data = data.frame(argvals = argvals_u$x), knots = NULL)
  n_evaluations <- table(data$id)
  underdet <- n_evaluations < spec_object$bs.dim
  if (!penalized) {
    if (any(underdet)) {
      warning("More basis functions than evaluations.", 
        " Interpolation will be unreliable.")
    }
    eval_list <- split(data$data, data$id)
    if (regular) {
      coef_list <- qr.coef(qr = qr(spec_object$X), 
        y = do.call(cbind, eval_list))
      coef_list <- split(coef_list, col(coef_list))
    } else {
      index_list <- split(attr(argvals_u, "index"), data$id)
      coef_list <- map2(index_list, eval_list, 
        ~ qr.coef(qr=qr(spec_object$X[.x,]), y = .y))
    }
    # need to remove NAs if dim(basis) > length(argvals)
    coef_list[underdet] <- map(coef_list[underdet], na_to_0) 
  } else {
    # CONTINUE HERE #
    # split(cbind(data$data, index = attr(argvals_u, "index")), id) %>%
      
    
    
  }
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


