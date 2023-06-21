#' Error bands using `tf` objects as bounds
#' 
#' Plots a shaded region between `tf`-objects `ymax` and `ymin`.
#' This is primarily intended to help with plotting confidence bands
#' although other purposes are possible. 
#'
#' @examples
#' set.seed(1221)
#' data =
#'   data.frame(id = factor(1:2))
#' data$f = tf_rgp(2)
#' data$ymax = data$f + 1
#' data$ymin = data$f - 1
#' library(ggplot2)
#' ggplot(data, aes(y = f, color = id)) +
#'   geom_spaghetti() +
#'   geom_errorband(aes(ymax = ymax, ymin = ymin, fill = id)) +
#'   facet_wrap(~id)
#' @name ggerrorband
NULL

#' @export
#' @importFrom ggplot2 ggproto Stat Geom
#' @rdname ggerrorband
#' @usage NULL
#' @format NULL
StatErrorband <- ggproto("StatErrorband", Stat,
  required_aes = c("ymax", "ymin"),
  setup_params = function(data, params) {
    if (is.null(params$arg)) {
      params$arg <- list(tf_arg(pull(data, ymax)))
    }
    params
  },
  compute_layer = function(self, data, params, layout) {
    stopifnot(is_tf(pull(data, ymax)) & is_tf(pull(data, ymin)))
    tf_eval <-
      suppressMessages(
        mutate(data, id = names(ymax) %||% seq_along(ymax)) %>%
          tf_unnest(c(ymax, ymin), .arg = params$arg, names_sep = "___")) %>%
      select(-group, -ymin___arg) %>%
      rename(group = id, x = ymax___arg, ymin = ymin___value, ymax = ymax___value)
    tf_eval
  },
  compute_panel = function(self, data, scales, arg, errorband) {
    Stat$compute_panel(self, data, scales)
  }
)

#' @export
#' @rdname ggerrorband
#' @inheritParams ggplot2::stat_identity
#' @param na.rm remove NAs? defaults to `TRUE`
stat_errorband <- function(mapping = NULL, data = NULL, geom = "errorband",
                    position = "identity", na.rm = TRUE, show.legend = NA,
                    inherit.aes = TRUE, arg = NULL, ...) {
  layer(
    stat = StatErrorband, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arg = arg, ...)
  )
}

# geom --------------------------------------------------------------------

#' @export
#' @rdname ggerrorband
#' @format NULL
#' @param arg where to evaluate `tf` -- defaults to the default ;)
geom_errorband <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = TRUE, show.legend = NA,
                           inherit.aes = TRUE, arg = NULL, ...) {
  layer(
    stat = StatErrorband, data = data, mapping = mapping, geom = "errorband",
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arg = arg, ...)
  )

}
#' @export
#' @rdname ggerrorband
#' @usage NULL
#' @format NULL
GeomErrorband <- ggproto("GeomErrorband", Geom,
  setup_params = function(data, params) {
     # TODO: implement proper "orientation" - see extending ggplot vignette
     params$flipped_aes <- FALSE
     params
  },
  setup_data = function(data, params) {
    GeomRibbon$setup_data(data, params)
  },
  draw_panel = function(data, panel_params, coord) {
    GeomRibbon$draw_panel(data, panel_params, coord)
  },
  default_aes = aes(
    fill = "grey70", linetype = 1, alpha = 0.3, linewidth = 0
  ),
  draw_key = GeomRibbon$draw_key,
  required_aes = c("ymin", "ymax")
)
