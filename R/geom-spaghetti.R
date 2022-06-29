#' Spaghetti plots for `tf` objects
#'
#' Plots a line for each entry of a `tf`-object.
#' `geom_spaghetti` does spaghetti plots, `geom_meatballs` does spaghetti plots
#' with points for the actual evaluations.
#'
#' @section `tf` aesthetic:
#'   Mandatory. Used to designate a column of class `tf` to be visualized.
#' @examples
#' set.seed(1221)
#' data = data.frame(col = sample(gl(5, 2)))
#' data$f = tf_rgp(10)
#' data$fi = tf_jiggle(data$f)
#' data$fb = tfb(data$f)
#' library(ggplot2)
#' ggplot(data, aes(tf = f, color = tf_depth(f))) + geom_spaghetti()
#' ggplot(data, aes(tf = fi, shape = col, color = col)) + geom_meatballs()
#' ggplot(data, aes(tf = fi)) + geom_meatballs(spaghetti = FALSE) +
#'   facet_wrap(~col)
#' @name ggspaghetti
#' @family tidyfun visualization
#' @import ggplot2
#' @seealso [geom_cappelini()] for glyph plots, [gglasagna()] for heatmaps.
NULL

#' @export
is.finite.tf <- function(x) {
  map_lgl(tf_evaluations(x), ~all(is.finite(.) | !is.na(.)))
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
  setup_params = function(data, params) {
    if (is.null(params$arg)) {
      params$arg <- list(tf_arg(pull(data, tf)))
    }
    params
  },
  compute_layer = function(self, data, params, layout) {
    stopifnot(is_tf(pull(data, tf)))
    tf_eval <- suppressMessages(
      mutate(data, tf____id = names(tf) %||% seq_along(tf)) %>% 
      tf_unnest(tf, .arg = params$arg, names_sep = "____")) %>%
      select(-group) %>%
      rename(group = tf____id, x = tf____arg, y = tf____value)
    tf_eval
  },
  # need this so arg, spaghetti gets recognized as valid parameters
  # because layer() only checks compute_panel & compute_group
  compute_panel = function(self, data, scales, arg, spaghetti) {
    Stat$compute_panel(self, data, scales)
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
#' @param arg where to evaluate `tf` -- defaults to the default ;)
geom_spaghetti <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = TRUE, show.legend = NA,
                           inherit.aes = TRUE, arg = NULL, ...) {
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
    colour = "black", size = 0.5,
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
#' @param spaghetti plot noodles along with meatballs? defaults to true.
geom_meatballs <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = TRUE, show.legend = NA,
                           inherit.aes = TRUE, arg = NULL, spaghetti = TRUE, 
                           ...) {
  ggplot2::layer(
    stat = StatTf, data = data, mapping = mapping, geom = "meatballs",
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arg = arg, spaghetti = spaghetti, ...)
  )
}
#' @export
#' @rdname ggspaghetti
#' @family tidyfun visualization
#' @usage NULL
#' @format NULL
GeomMeatballs <- ggplot2::ggproto("GeomMeatball", ggplot2::Geom,
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
    colour = "black", size = 0.5,
    linetype = 1, alpha = 0.5, shape = 19, fill = NA, stroke = 0.5
  ),
  draw_key = GeomLine$draw_key,
  required_aes = c("x", "y", "group")
)
