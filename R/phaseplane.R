#' Phase-plane representation of functional data
#'
#' Turns a univariate `tf` vector into a two-component [tf::tfd_mv()] /
#' [tf::tfb_mv()] object holding two derivatives of each function, so that
#' the curves can be displayed as trajectories in the plane spanned by these
#' derivatives -- a *phase-plane plot* like [fda::phaseplanePlot()].
#' By default, the first derivative (velocity, on the horizontal axis) is
#' paired with the second derivative (acceleration, on the vertical axis).
#' Use `order = c(0, 1)` for the classical phase portrait of position vs.
#' velocity.
#'
#' Derivatives are computed with [tf::tf_derive()], i.e. by finite differences
#' for `tfd` objects. Since differencing amplifies noise, phase-plane plots of
#' raw data are usually only informative for smooth functions (e.g. a
#' [tf::tfb()] representation, or [tf::tf_smooth()]ed data) on a fine grid.
#'
#' The result is a regular `tf_mv` object, so it can be plotted with
#' [tf_ggplot()] (map it with `aes(tf = ...)` and add [ggplot2::geom_path()])
#' or [autoplot()]. Use `aes(colour = .arg)` or `colour_by_arg = TRUE` to
#' colour the trajectories by their argument value, which is otherwise not
#' visible in a phase-plane plot.
#'
#' @param f a univariate `tf` object (`tfd` or `tfb`)
#' @param order integer vector of length 2: the derivative orders shown on the
#'   x- and y-axis. Defaults to `c(1, 2)` (velocity vs. acceleration); `0`
#'   means the function itself. Maximal order for [tf::tfb_spline()] objects
#'   is 2.
#' @param arg optional grid on which to evaluate the derivatives; defaults to
#'   `f`'s own grid, see [tf::tf_derive()].
#' @returns A two-component `tf_mv` object (`tfb_mv` for `tfb` input if both
#'   derivatives can be represented in basis form, otherwise `tfd_mv`) with
#'   components named `"D<order>"`, e.g. `"D1"` and `"D2"`.
#' @examples
#' library(ggplot2)
#' arg <- seq(0, 1, length.out = 101)
#' # sinusoids of varying frequency: the phase plane shows nested ellipses
#' f <- tfd(t(sapply(1:4, \(k) sin(2 * pi * k * arg))), arg = arg)
#' pp <- tf_phaseplane(f)
#' pp
#' autoplot(pp, colour_by_arg = TRUE)
#' # position vs. velocity, with tf_ggplot:
#' d <- data.frame(id = factor(1:4))
#' d$f <- f
#' tf_ggplot(d, aes(tf = tf_phaseplane(f, order = c(0, 1)), colour = id)) +
#'   geom_path() +
#'   labs(x = "position", y = "velocity")
#' @seealso [autoplot.tf_mv()], [tf::tf_derive()]
#' @family tidyfun visualization
#' @export
tf_phaseplane <- function(f, order = c(1L, 2L), arg = NULL) {
  if (!is_tf(f) || !tf::is_tf_1d(f)) {
    cli::cli_abort(
      "{.arg f} must be a univariate {.cls tf} object,
       not {.obj_type_friendly {f}}."
    )
  }
  if (
    !is.numeric(order) ||
      length(order) != 2L ||
      anyNA(order) ||
      any(order < 0) ||
      any(order != round(order))
  ) {
    cli::cli_abort(
      "{.arg order} must be a vector of 2 non-negative integers,
       not {.obj_type_friendly {order}}."
    )
  }
  if (order[1] == order[2]) {
    cli::cli_abort(
      "{.arg order} must contain two different derivative orders."
    )
  }
  order <- as.integer(order)
  derive <- function(o) {
    if (o == 0L) {
      return(f)
    }
    if (is.null(arg)) {
      tf_derive(f, order = o)
    } else {
      tf_derive(f, arg = arg, order = o)
    }
  }
  components <- stats::setNames(map(order, derive), paste0("D", order))
  if (all(map_lgl(components, is_tfb))) {
    return(tfb_mv(components))
  }
  # mixed representations (e.g. tfb with non-identity link derives to tfd):
  # coerce everything to tfd for a valid tf_mv
  components <- map(components, \(x) if (is_tfb(x)) tfd(x) else x)
  tfd_mv(components)
}
