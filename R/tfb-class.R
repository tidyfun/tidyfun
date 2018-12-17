#' Constructors & convertors for functional data in basis representation
#'
#' Various constructor and conversion methods.
#'
#' `tfb` is a wrapper for functions that set up spline-, principal component- 
#' or wavelet-based representations of functional data. For all three, the
#' input data $x_i(t)$ are represented as weighted sums of a set of common basis 
#' functions $B_k(t); k = 1,\\dots, K$ identical for all
#' observations and weight or coefficient vectors $b_i = (b_{i1}, \\dots, b_{iK})$ 
#' estimated for each observation: $x_i(t) = B_1(t) b_{i1} + \dots + B_K(t) b_{iK}$.
#' Depending on the value of `basis`, the basis functions $B(t)$ will either be 
#' `spline`` functions or the first few estimated eigenfunctions of the covariance 
#' operator of the $x(t)$ (`fpc`) or wavelets (`wavelet`).
#' 
#' See [tfb_spline()] for spline based basis representation (the default).
#' See [tfb_fpc()] for using an FPC representation with an orthonormal basis estimated from the
#' data instead.
#'
#' @param data a `matrix`, `data.frame` or `list` of suitable shape, or another
#'   `tf`-object containing functional data.
#' @param basis either "`spline`" (see [tfb_spline()], the default) or "`fpc`" (see [tfb_fpc()]). 
#'   (`wavelet` not implemented yet)
#' @param ... further arguments for [tfb_spline()] or [tfb_fpc()]
#' @return a `tfb`-object (or a `data.frame`/`matrix` for the conversion
#'   functions, obviously.)
#' @rdname tfb
#' @seealso [tfb_fpc()], [tfb_spline()]
#' @export
tfb <- function(data, basis = c("spline", "fpc", "wavelet"), ...) {
  basis <- match.arg(basis)
  switch(basis,
         spline  = tfb_spline(data, ...),
         fpc     = tfb_fpc(data, ...),
         wavelet = tfb_wavelet(data, ...))
}  

#' @rdname tfb
tfb_wavelet <- function(data, ...) .NotYetImplemented()

