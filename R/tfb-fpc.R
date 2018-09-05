# data -> FPC decomp: mean, efuns, scores, values, (pve/remainder) -> tfb_fpc representation
pc_truncated <- function(data, pve = .995) {
  mean <- colMeans(data)
  data_c <- t(t(data) - mean)
  pc <- svd(data_c, nu = min(dim(data)), nv = min(dim(data)))
  pve_observed <- cumsum(pc$d^2) / sum(pc$d^2)
  use <- min(which(pve_observed >= pve))
  efunctions <- pc$v[, 1:use]
  scores <- t(qr.coef(qr(efunctions), t(data_c)))
  list(
    mu = mean, efunctions = efunctions,
    scores = scores, npc = use
  )
}

fpc_wrapper <- function(efunctions) {
  function(arg) {
    t(efunctions[, arg, interpolate = TRUE, matrix = TRUE])
  }
}

#' @importFrom refund fpca.sc
make_tfb_fpc <- function(data, domain = NULL, smooth = TRUE, resolution = NULL, 
                         ...) {
  # FIXME: rm renaming once we've cleaned up fpca.sc etc
  # FIXME: warn if domain != range(arg), can't extrapolate FPCs
  arg <- sort(unique(data$arg))
  domain <- domain %||% range(arg)
  resolution <- resolution %||% get_resolution(arg)
  data$arg <- round_resolution(data$arg, resolution)
  arg <- unique(round_resolution(arg, resolution))
  names(data) <- c(".id", ".index", ".value")
  if (smooth) {
    fpca_args <-
      modifyList(
        list(ydata = data, nbasis = 15, pve = 0.995, useSymm = TRUE),
        list(...)[names(list(...)) %in% names(formals(refund::fpca.sc))]
      )
    fpc_spec <- do.call(fpca.sc, fpca_args)
  } else {
    datamat <- irreg2mat(data)
    fpca_args <-
      modifyList(
        list(data = datamat, pve = 0.995),
        list(...)[names(list(...)) %in% names(formals(pc_truncated))]
      )
    fpc_spec <- do.call(pc_truncated, fpca_args)
  }
  coef_list <- split(cbind(1, fpc_spec$scores), row(cbind(1, fpc_spec$scores)))
  names(coef_list) <- levels(as.factor(data$.id))
  fpc <- rbind(fpc_spec$mu, t(fpc_spec$efunctions))
  fpc_basis <- tfd(fpc,
    arg = arg, evaluator = tf_approx_spline, domain = domain,
    resolution = resolution
  )
  fpc_constructor <- fpc_wrapper(fpc_basis)
  structure(coef_list,
    domain = domain,
    basis = fpc_constructor,
    basis_label = paste0("FPC: ", fpc_spec$npc, " components."),
    basis_matrix = t(fpc),
    arg = arg,
    resolution = resolution,
    class = c("tfb_fpc", "tfb", "tf")
  )
}

#-------------------------------------------------------------------------------

#' Constructors for functional data in FPC-basis representation
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
#' @rdname tfb_fpc
#' @export
tfb_fpc <- function(data, ...) UseMethod("tfb_fpc")

#' @rdname tfb_fpc
#' @export
tfb_fpc.data.frame <- function(data, id = 1, arg = 2, value = 3,
                               domain = NULL, smooth = TRUE, resolution = NULL, 
                               ...) {
  data <- df_2_df(data, id, arg, value)
  make_tfb_fpc(data, domain = domain, smooth = smooth, 
               resolution = resolution, ...)
}

#' @rdname tfb_fpc
#' @export
tfb_fpc.matrix <- function(data, arg = NULL, domain = NULL, smooth = TRUE, 
                           resolution = NULL, ...) {
  arg <- unlist(find_arg(data, arg))
  names_data <- rownames(data)
  data <- mat_2_df(data, arg)
  ret <- make_tfb_fpc(data, domain = domain, smooth = smooth, 
                      resolution = resolution, ...)
  names(ret) <- names_data
  ret
}

#' @rdname tfb_fpc
#' @export
tfb_fpc.numeric <- function(data, arg = NULL, domain = NULL, smooth = TRUE, 
                            resolution = NULL, ...) {
  data <- t(as.matrix(data))
  tfb_fpc(data = data, arg = arg, smooth = smooth, domain = domain, 
          resolution = resolution, ...)
}

# #' @rdname tfb_fpc
# #' @export
# tfb_fpc.list <- function(data, arg = NULL, domain = NULL, smooth = TRUE,
# TODO

#' @rdname tfb_fpc
#' @export
tfb_fpc.tf <- function(data, arg = NULL, smooth = TRUE, ...) {
  # TODO: major computational shortcuts possible here for tfb: reduced rank,
  #   direct inner prods of basis functions etc...
  arg <- arg %||% tf_arg(data)
  names_data <- names(data)
  ret <- tfb_fpc(as.data.frame(data, arg = arg),
    smooth = smooth,
    domain = tf_domain(data), resolution = tf_resolution(data), ...
  )
  names(ret) <- names_data
  ret
}
