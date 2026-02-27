#' Autoplot and autolayer methods for `tf` objects
#'
#' Convenient plotting methods for `tf` objects. `autoplot()` creates a
#' complete spaghetti plot, `autolayer()` creates a spaghetti layer that can be
#' added to an existing [ggplot2::ggplot()].
#'
#' @param object a `tf` object
#' @param ... passed to [geom_spaghetti()]
#' @returns A [ggplot2::ggplot()] object for `autoplot()`, a [ggplot2::layer()] object for `autolayer()`.
#' @examples
#' \donttest{
#' library(ggplot2)
#' f <- tf_rgp(5)
#' autoplot(f)
#' ggplot() + autolayer(f)
#' }
#' @name autoplot.tf
#' @family tidyfun visualization
NULL

#' @export
#' @rdname autoplot.tf
autoplot.tf <- function(object, ...) {
  data <- tibble(y = object)
  ggplot(data, aes(y = .data$y)) + geom_spaghetti(...)
}

#' @export
#' @rdname autoplot.tf
autolayer.tf <- function(object, ...) {
  data <- tibble(y = object)
  geom_spaghetti(mapping = aes(y = .data$y), data = data, ..., inherit.aes = FALSE)
}
