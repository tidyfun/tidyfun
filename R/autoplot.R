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
#' Trajectory plots lose the information about *where* along the domain a
#' point lies. Set `colour_by_arg = TRUE` to colour each segment of the
#' trajectories by its argument value. This maps the `.arg` column that
#' [tf_ggplot()] provides to the `colour` aesthetic, so
#' `autoplot(mv, colour_by_arg = TRUE)` is equivalent to
#' `tf_ggplot(data, aes(tf = mv, colour = .arg)) + geom_path()`.
#'
#' @param object a `tf_mv` object
#' @param ... passed to [geom_path()] (trajectory) or [geom_line()] (facet)
#' @param type `"trajectory"`, `"facet"`, or `NULL` to resolve from the number of
#'   components (see Details).
#' @param colour_by_arg colour the segments of trajectories by their argument
#'   value? Defaults to `FALSE`. Only available for `type = "trajectory"`.
#' @returns A [tf_ggplot()] object for `autoplot()`, a [ggplot2::layer()] for
#'   `autolayer()`.
#' @examplesIf rlang::is_installed("ggplot2")
#' library(ggplot2)
#' mv <- tfd_mv(list(x = tf_rgp(5), y = tf_rgp(5)))
#' autoplot(mv)
#' autoplot(mv, colour_by_arg = TRUE)
#' ggplot() + autolayer(mv)
#' # phase-plane plot of a univariate function (velocity vs. acceleration):
#' f <- tf_rgp(3, arg = 101L, nugget = 0)
#' autoplot(tf_phaseplane(f), colour_by_arg = TRUE)
#' @name autoplot.tf_mv
#' @family tidyfun visualization
#' @seealso [tf_phaseplane()] for phase-plane plots of univariate functions.
NULL

#' @export
#' @rdname autoplot.tf_mv
autoplot.tf_mv <- function(object, ..., type = NULL, colour_by_arg = FALSE) {
  type <- resolve_tf_mv_type(type, tf_ncomp(object))
  check_colour_by_arg(colour_by_arg, type)
  data <- tibble::tibble(mv = object)
  if (type == "trajectory") {
    mapping <- aes(tf = .data$mv)
    if (colour_by_arg) {
      mapping$colour <- rlang::quo(.data$.arg)
    }
    p <- tf_ggplot(data, mapping, type = "trajectory") + geom_path(...)
    if (colour_by_arg) {
      p$labels$colour <- "arg"
    }
    p
  } else {
    tf_ggplot(data, aes(tf = .data$mv), type = "facet") +
      geom_line(...) +
      facet_wrap(~.component)
  }
}

#' @export
#' @rdname autoplot.tf_mv
autolayer.tf_mv <- function(object, ..., type = NULL, colour_by_arg = FALSE) {
  type <- resolve_tf_mv_type(type, tf_ncomp(object))
  check_colour_by_arg(colour_by_arg, type)
  if (type == "trajectory") {
    df <- .tf_mv_trajectory_long(object)
    df$.arg <- df$.mv_arg
    mapping <- aes(
      x = .data$.mv_x,
      y = .data$.mv_y,
      group = .data$.mv_id
    )
    if (colour_by_arg) {
      mapping$colour <- rlang::quo(.data$.arg)
    }
    geom_path(
      mapping = mapping,
      data = df,
      ...,
      inherit.aes = FALSE
    )
  } else {
    df <- .tf_mv_unnest_long(object)
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

check_colour_by_arg <- function(colour_by_arg, type, call = rlang::caller_env()) {
  if (!rlang::is_bool(colour_by_arg)) {
    cli::cli_abort(
      "{.arg colour_by_arg} must be {.code TRUE} or {.code FALSE},
       not {.obj_type_friendly {colour_by_arg}}.",
      call = call
    )
  }
  if (colour_by_arg && type != "trajectory") {
    cli::cli_abort(
      c(
        "{.code colour_by_arg = TRUE} is only available for
         {.code type = \"trajectory\"}.",
        i = "In {.code type = \"facet\"} displays the argument is already shown
         on the x-axis."
      ),
      call = call
    )
  }
  invisible(colour_by_arg)
}
