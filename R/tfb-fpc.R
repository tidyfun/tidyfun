# data -> FPC decomp: mean, efuns, scores, values, (pve/remainder) -> fpcbase representation
pc_truncated <- function(data, pve = .995) {
  mean <- colMeans(data)
  data_c <- t(t(data) - mean)
  pc <- svd(data_c, nu = ncol(data), nv = ncol(data))
  pve_observed <- cumsum(pc$d^2)/sum(pc$d^2)
  use <- min(which(pve_observed >= pve))
  efunctions <- pc$v[,1:use]
  scores <- t(qr.coef(qr(efunctions), t(data_c)))
  list(mu = mean, efunctions = efunctions, 
    scores = scores, npc = use)
}


fpc_wrapper <- function(efunctions) {
  function(argvals) {
    t(efunctions[, argvals, interpolate = TRUE, matrix = TRUE])
  }
} 

#' @importFrom refund fpca.sc
fpc_tfb <- function(data, domain = NULL, smooth = TRUE, signif = 4, ...) {
  #FIXME: rm renaming once we've cleaned up fpca.sc etc
  #FIXME: warn if domain != range(argvals), can't extrapolate FPCs
  data$argvals <- .adjust_resolution(data$argvals, signif, unique = FALSE)
  argvals <- sort(unique(data$argvals))
  names(data) <- c(".id", ".index", ".value")
  if (smooth) {
    fpca_args <- 
      modifyList(list(ydata = data, nbasis = 15, pve = 0.995, useSymm = TRUE),
        list(...)[names(list(...)) %in% names(formals(refund::fpca.sc))])
    fpc_spec <- do.call(fpca.sc, fpca_args)
  } else {
    datamat <- refund:::irreg2mat(data)
    fpca_args <- 
      modifyList(list(data = datamat, pve = 0.995),
        list(...)[names(list(...)) %in% names(formals(pc_truncated))])
    fpc_spec <- do.call(pc_truncated, fpca_args)
  }
  coef_list <- split(cbind(1, fpc_spec$scores), row(cbind(1, fpc_spec$scores)))
  names(coef_list) <- levels(as.factor(data$.id))
  fpc <- rbind(fpc_spec$mu, t(fpc_spec$efunctions))
  fpc_basis <- tfd(fpc, argvals = argvals, evaluator = approx_spline)
  fpc_constructor <- fpc_wrapper(fpc_basis)
  structure(coef_list, 
    domain = domain %||% range(argvals),
    basis = fpc_constructor,
    basis_label = paste0("FPC: ", fpc_spec$npc, " components."),
    basis_matrix = t(fpc),
    argvals = argvals,
    signif_argvals = signif, 
    class = c("tfb", "tf"))
}

#-------------------------------------------------------------------------------

#' Constructors/converters for functional data in FPC-basis representation
#' 
#' These functions perform a (functional) principal component analysis of the
#' input data and return an `tfb` `tf`-object that uses the empirical 
#' eigenfunctions as basis functions for representing the data. By default, a
#' `smooth`ed FPCA via [refund::fpca.sc()] is used to compute eigenfunctions and 
#' scores based on the smoothed empirical covariance. 
#' If `smooth =FALSE`, a fast, unregularized PCA of the data is done instead.
#' 
#' ATM, the unsmoothed version does not use orthogonal basis "functions" 
#'   for irregular / non-equidistant grids and does not work for 
#'   incomplete/irregular data.
#' @export
#' @inheritParams tfd.data.frame
#' @param smooth use smoothed mean function and smoothed covariance surface estimates
#'   from [refund::fpca.sc()] or simply perform a (truncated) PCA of the data? See Details.
#' @param ... arguments to the call to [refund::fpca.sc()] that computes the
#'  (regularized/smoothed) FPCA. Unless set by the user `tidyfun` uses `pve = .995` to 
#'  determine the truncation levels and uses `bs = 15` basis functions for 
#'  the mean function and the marginal bases for the covariance surface.
#' @seealso tfb
#' @rdname fpcbase
#' @export
fpcbase <- function(data, ...) UseMethod("fpcbase")

#' @rdname fpcbase
#' @export
fpcbase.data.frame <- function(data, id = 1, argvals = 2, value = 3,  
  domain = NULL, smooth = TRUE, signif = 4, ...) {
  data <- df_2_df(data, id, argvals, value)
  fpc_tfb(data, domain = domain, signif = signif, ...)
}

#' @rdname fpcbase
#' @export
fpcbase.matrix <- function(data, argvals = NULL, domain = NULL, smooth = TRUE, signif = 4, ...) {
  argvals <- unlist(find_argvals(data, argvals))
  names_data <- rownames(data)
  data <- mat_2_df(data, argvals)
  ret <- fpc_tfb(data, domain = domain, smooth = smooth, signif = signif, ...)
  names(ret) <- names_data
  ret
}

#' @rdname fpcbase
#' @export
fpcbase.numeric <- function(data, argvals = NULL, domain = NULL, smooth = TRUE, signif = 4, ...) {
  data <- t(as.matrix(data))
  fpcbase(data = data, argvals = argvals, domain = domain, smooth = smooth, signif = signif, ...)
}

# #' @rdname fpcbase
# #' @export
# fpcbase.list <- function(data, argvals = NULL, domain = NULL, smooth = TRUE, 
# TODO

#' @rdname fpcbase
#' @export
fpcbase.tf <- function(data, argvals = NULL, smooth = TRUE, ...) {
   #TODO: major computational shortcuts possible here for tfb: reduced rank,
  #   direct inner prods of basis functions etc...
  argvals <- argvals %||% argvals(data)
  names_data <- names(data)
  ret <- fpcbase(as.data.frame(data, argvals = argvals), smooth = smooth, 
    signif = attr(data, "signif_argvals"),  ...)
  names(ret) <- names_data
  ret
}
