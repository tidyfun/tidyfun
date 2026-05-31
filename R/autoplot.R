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

#' Autoplot and autolayer methods for multivariate (`tf_mv`) objects
#'
#' Plotting methods for vector-valued functional data (`tf_mv`, functions
#' \eqn{R \to R^d}{R -> R^d}), mirroring the display modes of [tf::plot.tf_mv()].
#'
#' For two-component objects (`d == 2`) the default is a `"trajectory"` plot of
#' the planar curve x(t) vs y(t) (drawn with [geom_path()], which connects in
#' argument order). Otherwise the default is `"facet"`: value-vs-arg curves with
#' one panel per output dimension. `autolayer()` returns a single layer (no
#' faceting) and works with plain [ggplot2::ggplot()] as well as [tf_ggplot()].
#'
#' @param object a `tf_mv` object
#' @param ... passed to [geom_path()] (trajectory) or [geom_line()] (facet)
#' @param type `"trajectory"`, `"facet"`, or `NULL` to resolve from the number of
#'   components (see Details).
#' @returns A [tf_ggplot()] object for `autoplot()`, a [ggplot2::layer()] for
#'   `autolayer()`.
#' @examplesIf rlang::is_installed("ggplot2")
#' library(ggplot2)
#' mv <- tfd_mv(list(x = tf_rgp(5), y = tf_rgp(5)))
#' autoplot(mv)
#' ggplot() + autolayer(mv)
#' @name autoplot.tf_mv
#' @family tidyfun visualization
NULL

#' @export
#' @rdname autoplot.tf_mv
autoplot.tf_mv <- function(object, ..., type = NULL) {
  type <- match.arg(
    type %||% if (tf_ncomp(object) == 2L) "trajectory" else "facet",
    c("trajectory", "facet")
  )
  data <- tibble::tibble(mv = object)
  if (type == "trajectory") {
    tf_ggplot(data, aes(tf = .data$mv), type = "trajectory") + geom_path(...)
  } else {
    tf_ggplot(data, aes(tf = .data$mv), type = "facet") +
      geom_line(...) +
      facet_wrap(~.component)
  }
}

#' @export
#' @rdname autoplot.tf_mv
autolayer.tf_mv <- function(object, ..., type = NULL) {
  type <- match.arg(
    type %||% if (tf_ncomp(object) == 2L) "trajectory" else "facet",
    c("trajectory", "facet")
  )
  if (type == "trajectory") {
    if (tf_ncomp(object) != 2L) {
      cli::cli_abort(
        "{.code type = \"trajectory\"} requires a {.cls tf_mv} with exactly 2 components."
      )
    }
    df <- .tf_mv_trajectory_long(object)
    geom_path(
      mapping = aes(
        x = .data$.mv_x,
        y = .data$.mv_y,
        group = .data$.mv_id
      ),
      data = df,
      ...,
      inherit.aes = FALSE
    )
  } else {
    df <- .tf_mv_unnest_long(object)
    grp <- paste(df$id, df$.component, sep = ".")
    df$.mv_group <- ordered(grp, levels = unique(grp))
    geom_line(
      mapping = aes(
        x = .data$arg,
        y = .data$value,
        group = .data$.mv_group
      ),
      data = df,
      ...,
      inherit.aes = FALSE
    )
  }
}
