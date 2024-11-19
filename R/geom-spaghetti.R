#' Spaghetti plots for `tf` objects
#'
#' Plots a line for each entry of a `tf`-object.
#' `geom_spaghetti` does spaghetti (i.e. "hairball") plots, `geom_polpette`
#' does line plots with *polpette*-points for the actual evaluations.
#'
#' "Flipped" aesthetics are not implemented for these geoms.
#'
#' @section `tf` aesthetic:
#'   Mandatory. Used to designate a column of class `tf` to be visualized.
#'   (You'll need to title the `y`-axis manually, sorry.)
#' @examples
#' set.seed(1221)
#' data <- data.frame(col = sample(gl(5, 2)))
#' data$f <- tf_rgp(10)
#' data$fi <- tf_jiggle(data$f)
#' data$fb <- tfb(data$f)
#' library(ggplot2)
#' ggplot(data, aes(tf = f, color = tf_depth(f))) +
#'   geom_spaghetti()
#' ggplot(data, aes(tf = fi, shape = col, color = col)) +
#'   geom_polpette()
#' ggplot(data, aes(tf = fi)) +
#'   geom_polpette(spaghetti = FALSE) +
#'   facet_wrap(~col)
#' @name ggspaghetti
#' @family tidyfun visualization
#' @import ggplot2
#' @seealso [geom_cappelini()] for glyph plots, [gglasagna()] for heatmaps.
NULL

#' @export
is.finite.tf <- function(x) {
  map_lgl(tf_evaluations(x), \(x) all(is.finite(x) | !is.na(x)))
}

#' @export
scale_type.tf <- function(x) "identity"

#' @export
#' @importFrom ggplot2 ggproto Stat Geom
#' @importFrom dplyr pull
#' @rdname ggspaghetti
#' @family tidyfun visualization
#' @usage NULL
#' @format NULL
StatTf <- ggproto("StatTf", Stat,
  required_aes = "tf",
   # need _panel not _layer because _layer does not get "scales"
  # and scale-transforms to x-y need to be applied here!
  compute_panel = function(self, data, scales, arg, spaghetti) {
    stopifnot(is_tf(pull(data, tf)))
    if (is.null(arg)) {
      arg <- list(tf_arg(pull(data, tf)))
    }
    tf_eval <- suppressMessages(
      data |>
        mutate(spghtt_id = names(tf) %||% seq_along(tf)) |>
        tf_unnest(tf, arg = arg, names_sep = "_spghtt_")
    ) |>
      select(-group) |>
      rename(group = spghtt_id, x = tf_spghtt_arg, y = tf_spghtt_value)
    if (!is.null(scales$x)) {
      tf_eval$x <- tf_eval$x |> scales$x$get_transformation()$transform()
    }
    if (!is.null(scales$y)) {
      tf_eval$y <- tf_eval$y |> scales$y$get_transformation()$transform()
    }
    tf_eval
  }
)

#' @export
#' @rdname ggspaghetti
#' @family tidyfun visualization
#' @inheritParams ggplot2::stat_identity
#' @param na.rm remove NAs? defaults to `TRUE`
stat_tf <- function(mapping = NULL, data = NULL, geom = "spaghetti",
                    position = "identity", na.rm = TRUE, show.legend = NA,
                    inherit.aes = TRUE, arg = NULL, ...) {
  ggplot2::layer(
    stat = StatTf, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arg = arg, ...)
  )
}

# geom --------------------------------------------------------------------

#' @export
#' @rdname ggspaghetti
#' @family tidyfun visualization
#' @format NULL
#' @param arg where to evaluate the functions in `y` -- defaults to the default ;)
geom_spaghetti <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = TRUE, show.legend = NA,
                           inherit.aes = TRUE, arg = NULL, ...) {
  if ("y" %in% names(mapping)) {
    warning("using 'aes(y = ...)' is deprecated, use `aes(tf = ...)` instead")
    names(mapping) <- sub("^y$", "tf", names(mapping))
  }
  ggplot2::layer(
    stat = StatTf, data = data, mapping = mapping, geom = "spaghetti",
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arg = arg, ...)
  )
}
#' @export
#' @rdname ggspaghetti
#' @family tidyfun visualization
#' @usage NULL
#' @format NULL
GeomSpaghetti <- ggplot2::ggproto("GeomSpaghetti", ggplot2::Geom,
  setup_params = function(data, params) {
    # TODO: implement proper "orientation" - see extending ggplot vignette
    params$flipped_aes <- FALSE
    params
  },
  setup_data = function(data, params) {
    GeomLine$setup_data(data, params)
  },
  draw_group = function(data, panel_params, coord) {
    GeomLine$draw_panel(data, panel_params, coord)
  },
  default_aes = ggplot2::aes(
    colour = "black", linewidth = 0.5,
    linetype = 1, alpha = 0.5
  ),
  draw_key = GeomLine$draw_key,
  required_aes = c("x", "y", "group")
)

#-------------------------------------------------------------------------------

#' @export
#' @rdname ggspaghetti
#' @family tidyfun visualization
#' @format NULL
#' @importFrom grid gList
#' @param spaghetti plot lines along with points? defaults to TRUE.
geom_polpette <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = TRUE, show.legend = NA,
                           inherit.aes = TRUE, arg = NULL, spaghetti = TRUE,
                           ...) {
  if ("y" %in% names(mapping)) {
    warning("using 'aes(y = ...)' is deprecated, use `aes(tf = ...)` instead")
    names(mapping) <- sub("^y$", "tf", names(mapping))
  }
  ggplot2::layer(
    stat = StatTf, data = data, mapping = mapping, geom = "polpette",
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arg = arg, spaghetti = spaghetti, ...)
  )
}
#' @export
#' @rdname ggspaghetti
#' @family tidyfun visualization
#' @usage NULL
#' @format NULL
GeomPolpette <- ggplot2::ggproto("GeomPolpette", ggplot2::Geom,
  setup_params = function(data, params) {
    # TODO: implement proper "orientation" - see extending ggplot vignette
    params$flipped_aes <- FALSE
    params
  },
  setup_data = function(data, params) {
    GeomLine$setup_data(data, params)
  },
  draw_group = function(data, panel_params, coord, spaghetti = TRUE) {
    grid::gList(
      if (spaghetti) GeomLine$draw_panel(data, panel_params, coord),
      GeomPoint$draw_panel(data, panel_params, coord)
    )
  },
  default_aes = ggplot2::aes(
    colour = "black", linewidth = 0.5, size = 0.5,
    linetype = 1, alpha = 0.5, shape = 19, fill = NA, stroke = 0.5
  ),
  draw_key = GeomLine$draw_key,
  required_aes = c("x", "y", "group")
)
