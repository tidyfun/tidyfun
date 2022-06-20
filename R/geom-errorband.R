#' Error bands based on `tf` objects for bounds
#'
#' @examples
#' set.seed(1221)
#' data =
#'   data.frame(ID = 1:2)
#' data$f = tf_rgp(2)
#' data$UB = data$f + 1
#' data$LB = data$f - 1
#' library(ggplot2)
#' ggplot(data, aes(y = f, color = ID)) +
#'   geom_spaghetti() +
#'   geom_ribbon(aes(ub = UB, lb = LB, fill = ID, color = NULL), alpha = .3, stat = "errorband") +
#'   facet_wrap(vars(ID))
#' @name ggerrorband
NULL

#' @export
#' @importFrom ggplot2 ggproto Stat Geom
#' @rdname ggerrorband
#' @usage NULL
#' @format NULL
StatErrorband <- ggproto("StatErrorband", Stat,
  required_aes = c("ub", "lb"),
  setup_params = function(data, params) {
    if (is.null(params$arg)) {
      params$arg <- list(tf_arg(pull(data, ub)))
    }
    params
  },
  compute_layer = function(self, data, params, layout) {
    stopifnot(is_tf(pull(data, ub)) & is_tf(pull(data, lb)))
    tf_eval <-
      suppressMessages(
        mutate(data, id = names(ub) %||% seq_along(ub)) %>%
          tf_unnest(c(ub, lb), .arg = params$arg, names_sep = "___")) %>%
      select(-group) %>%
      rename(group = id, x = arg, ymin = lb___value, ymax = ub___value)
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
stat_errorband <- function(mapping = NULL, data = NULL, geom = "errorbar",
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
  setup_data = function(data, params) {
    GeomRibbon$setup_data(data, params)
  },
  draw_panel = function(data, panel_params, coord) {
    GeomRibbon$draw_panel(data, panel_params, coord)
  },
  default_aes = aes(
    fill = "grey70", linetype = 1, alpha = 0.3
  ),
  draw_key = GeomRibbon$draw_key,
  required_aes = c("ub", "lb")
)
