#' Autoplot and autolayer methods for `tf` objects
#'
#' Convenient plotting methods for `tf` objects. `autoplot()` creates a
#' complete spaghetti plot, `autolayer()` creates a layer that can be
#' added to an existing [ggplot2::ggplot()] or [tf_ggplot()].
#'
#' @param object a `tf` object
#' @param ... passed to [geom_line()]
#' @returns A [tf_ggplot()] object for `autoplot()`, a [ggplot2::layer()] object for `autolayer()`.
#' @examples
#' \donttest{
#' library(ggplot2)
#' f <- tf_rgp(5)
#' autoplot(f)
#' ggplot() + autolayer(f)
#' tf_ggplot() + autolayer(f)
#' }
#' @name autoplot.tf
#' @family tidyfun visualization
NULL

#' @export
#' @rdname autoplot.tf
autoplot.tf <- function(object, ...) {
  data <- tibble::tibble(tf = object)
  tf_ggplot(data, aes(tf = .data$tf)) + geom_line(...)
}

#' @export
#' @rdname autoplot.tf
autolayer.tf <- function(object, ...) {
  # geom_spaghetti works with plain ggplot(); when added to tf_ggplot() it is
  # automatically translated to geom_line(aes(tf = ...)) by +.tf_ggplot.
  data <- tibble::tibble(y = object)
  geom_spaghetti(
    mapping = aes(y = .data$y),
    data = data,
    ...,
    inherit.aes = FALSE
  )
}
