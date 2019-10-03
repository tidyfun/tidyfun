#' @importFrom refund fpca.sc
new_tfb_fpc <- function(data, domain = NULL, resolution = NULL, 
                        method = NULL, ...) {
  
  arg <- sort(unique(data$arg))
  resolution <- resolution %||% get_resolution(arg)
  data$arg <- round_resolution(data$arg, resolution)
  arg <- unique(round_resolution(arg, resolution))
  
  domain <- domain %||% range(arg)
  domain <- c(round_resolution(domain[1], resolution, -1),
              round_resolution(domain[2], resolution, 1))
  if (!isTRUE(all.equal(domain, range(arg), 
                        tolerance = resolution, scale = 1))) {
    warning("domain for tfb_fpc can't be larger than observed arg-range --",
            " extrapolating FPCs is a bad idea.\n domain reset to [", min(arg), 
            ",", max(arg),"]")
    domain <- range(arg)
  }
  
  
  fpc_args <- get_args(list(...), method)
  fpc_args <- c(fpc_args, list(data = data, arg = arg))
  fpc_spec <- do.call(method, fpc_args)
  coef_list <- split(cbind(1, fpc_spec$scores), row(cbind(1, fpc_spec$scores)))
  names(coef_list) <- levels(as.factor(data$id))
  fpc <- rbind(fpc_spec$mu, t(fpc_spec$efunctions))
  fpc_basis <- tfd(fpc, arg = arg, domain = domain, resolution = resolution)
  fpc_constructor <- fpc_wrapper(fpc_basis)
  structure(coef_list,
    domain = domain,
    basis = fpc_constructor,
    basis_label = paste0(fpc_spec$npc, " FPCs"),
    basis_matrix = t(fpc),
    arg = arg,
    resolution = resolution,
    class = c("tfb_fpc", "tfb", "tf")
  )
}

#-------------------------------------------------------------------------------

#' Functional data in FPC-basis representation
#'
#' These functions perform a (functional) principal component analysis (FPCA) of
#' the input data and return an `tfb_fpc` `tf`-object that uses the empirical
#' eigenfunctions as basis functions for representing the data. By default, a
#' simple, not smoothed, truncated weighted SVD of the functions is used to
#' compute those ("`method = fpc_wsvd`"). Note that this is suitable only for
#' regular data all observed on the same (not necessarily equidistant) grid. See
#' Details / Example for possible alternatives and extensions. \cr
#' 
#' Any "factorization" method that accepts a `data.frame` with 
#' columns `id`, `arg`, `value` containing the functional data and returns a 
#' list structured like the return object
#' of [fpc_wsvd()] can be used for the `method`` argument, see example below.
#' 
#' @export
#' @inheritParams tfd.data.frame
#' @param method the function to use that computes eigenfunctions and scores.
#'   Defaults to [fpc_wsvd()], which gives unsmoothed eigenfunctions. 
#' @param ... arguments to the `method` which computes the
#'  (regularized/smoothed) FPCA. 
#'  Unless set by the user `tidyfun` uses proportion of variance explained 
#'  `pve = .995` to determine the truncation levels.
#' @return an object of class `tfb_fpc`, inheriting from `tfb`. 
#'    The basis used by `tfb_fpc` is a `tfd`-vector containing the estimated
#'    mean and eigenfunctions.
#' @seealso [tfb()],  [fpc_wsvd()] for FPCA options. 
#' @rdname tfb_fpc
#' @export
tfb_fpc <- function(data, ...) UseMethod("tfb_fpc")

#' @rdname tfb_fpc
#' @export
#' @examples 
#' # Apply FPCA for sparse data using refund::fpca.sc:
#' set.seed(99290)
#' # create sparse data:
#' data <- as.data.frame(
#'   tf_sparsify(
#'     tf_rgp(15)
#' ))
#' # wrap refund::fpca_sc for use as FPCA method in tfb_fpc:
#' fpca_sc_wrapper <- function(data, arg, pve = .995, ...) {
#'   data_mat <- tidyfun:::df_2_mat(data)
#'   fpca <- refund::fpca.sc(Y = data_mat, 
#'                           argvals = attr(data_mat, "arg"), 
#'                           pve = pve, ...)
#'   fpca[c("mu", "efunctions", "scores", "npc")]
#' }
#' tfb_fpc(data, method = fpca_sc_wrapper)
tfb_fpc.data.frame <- function(data, id = 1, arg = 2, value = 3,
                               domain = NULL, method = fpc_wsvd, resolution = NULL, 
                               ...) {
  data <- df_2_df(data, id, arg, value)
  new_tfb_fpc(data, domain = domain, method = method, 
               resolution = resolution, ...)
}

#' @rdname tfb_fpc
#' @export
tfb_fpc.matrix <- function(data, arg = NULL, domain = NULL, method = fpc_wsvd, 
                           resolution = NULL, ...) {
  arg <- unlist(find_arg(data, arg))
  names_data <- rownames(data)
  data <- mat_2_df(data, arg)
  ret <- new_tfb_fpc(data, domain = domain, method = method,
                      resolution = resolution, ...)
  names(ret) <- names_data
  ret
}

#' @rdname tfb_fpc
#' @export
tfb_fpc.numeric <- function(data, arg = NULL, domain = NULL, method = fpc_wsvd, 
                            resolution = NULL, ...) {
  data <- t(as.matrix(data))
  tfb_fpc(data = data, arg = arg, method = method, domain = domain, 
          resolution = resolution, ...)
}

# #' @rdname tfb_fpc
# #' @export
# tfb_fpc.list <- function(data, arg = NULL, domain = NULL, method = fpc_wsvd,
# TODO

#' @rdname tfb_fpc
#' @export
tfb_fpc.tf <- function(data, arg = NULL, method = fpc_wsvd, ...) {
  # TODO: major computational shortcuts possible here for tfb: reduced rank,
  #   direct inner prods of basis functions etc...
  arg <- arg %||% tf_arg(data)
  names_data <- names(data)
  ret <- tfb_fpc(as.data.frame(data, arg = arg),
    method = method,
    domain = tf_domain(data), resolution = tf_resolution(data), ...
  )
  names(ret) <- names_data
  ret
}
