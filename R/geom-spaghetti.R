#' Spaghetti plots for `tf` objects
#' 
#' Plots a line for each entry of a `tf`-object.  
#' `geom_spaghetti` does spaghetti plots, `geom_meatballs` does spaghetti plots
#' with points for the actual evaluations.
#' 
#' @section `y` aesthetic:
#'   Mandatory. Used to designate a column of class `tf` to be visualized. 
#' @examples
#' set.seed(1221)
#' data = data.frame(col = sample(gl(5, 2)))
#' data$f = rgp(10)
#' data$fi = jiggle(data$f)
#' data$fb = tfb(data$f)
#' library(ggplot2)
#' ggplot(data, aes(y = f, color = depth(f))) + geom_spaghetti()
#' ggplot(data, aes(y = fi, shape = col, color = col)) + geom_meatballs()
#' ggplot(data, aes(y = fi)) + geom_meatballs(spaghetti = FALSE) + 
#'   facet_wrap(~col)
#' @name ggspaghetti
NULL

# FIXME
# > ggplot(data, aes(y = f, color = depth(f))) + geom_spaghetti() +
#   + annotate("text", x = 1.05, y = runif(10), label = 1:10)
# Error: evaluation nested too deeply: infinite recursion / options(expressions=)?
#   Error during wrapup: evaluation nested too deeply: infinite recursion / options(expressions=)?

#' @export
is.finite.tf <- function(x) map(evaluations(x), ~ all(is.finite(x) | !is.na(x)))

#' @export
scale_type.tf <- function(x) "identity"

#' @export
#' @importFrom ggplot2 ggproto Stat Geom
#' @rdname ggspaghetti
#' @usage NULL
#' @format NULL
StatTf <- ggproto("StatTf", Stat,
  required_aes = "y",
  setup_params = function(data, params) {
    if (is.null(params$arg))
      params$arg <- list(arg(pull(data, y)))
    params
  },
  compute_layer = function(self, data, params, layout) {
    stopifnot(is_tf(pull(data, y)))
    tf_eval <- 
      suppressMessages(tf_unnest(data, y, .arg = params$arg, .sep = "___")) %>%
      select(-group) %>% 
      rename(group = y___id, x = y___arg, y = y___value)
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
#' @inheritParams ggplot2::stat_identity
#' @param na.rm remove NAs? defaults to `TRUE`
stat_tf <- function(mapping = NULL, data = NULL, geom = "spaghetti",
  position = "identity", na.rm = TRUE, show.legend = NA, 
  inherit.aes = TRUE, arg = NULL, ...) {
  layer(
    stat = StatTf, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arg = arg, ...)
  )
}

# geom --------------------------------------------------------------------

#' @export
#' @rdname ggspaghetti
#' @format NULL
#' @param arg where to evaluate `tf` -- defaults to the default ;)
geom_spaghetti <- function(mapping = NULL, data = NULL,
  position = "identity", na.rm = TRUE, show.legend = NA, 
  inherit.aes = TRUE, arg = NULL, ...) {
  layer(
    stat = StatTf, data = data, mapping = mapping, geom = "spaghetti", 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arg = arg, ...)
  )
}
#' @export
#' @rdname ggspaghetti
#' @usage NULL
#' @format NULL
GeomSpaghetti <- ggproto("GeomSpaghetti", Geom,
  setup_data = function(data, params) {
    GeomLine$setup_data(data, params)
  },
  draw_group = function(data, panel_params, coord) {
    GeomLine$draw_panel(data, panel_params, coord)
  },
  default_aes = aes(colour = "black", size = 0.5,
    linetype = 1, alpha = 0.5),
  draw_key = GeomLine$draw_key,
  required_aes = c("y")
)


#' @export
#' @rdname ggspaghetti
#' @format NULL
#' @importFrom grid gList
#' @param spaghetti plot noodles along with meatballs? defaults to true.
geom_meatballs <- function(mapping = NULL, data = NULL,
  position = "identity", na.rm = TRUE, show.legend = NA, 
  inherit.aes = TRUE, arg = NULL, spaghetti = TRUE, ...) {
  layer(
    stat = StatTf, data = data, mapping = mapping, geom = "meatballs", 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arg = arg, spaghetti = spaghetti, ...)
  )
}
#' @export
#' @rdname ggspaghetti
#' @usage NULL
#' @format NULL
GeomMeatballs <- ggproto("GeomMeatball", Geom,
  setup_data = function(data, params) {
    GeomLine$setup_data(data, params)
  },
  draw_group = function(data, panel_params, coord, spaghetti = TRUE) {
    grid::gList(
      if (spaghetti) GeomLine$draw_panel(data, panel_params, coord),
      GeomPoint$draw_panel(data, panel_params, coord))
  },
  default_aes = aes(colour = "black", size = 0.5,
    linetype = 1, alpha = 0.5, shape = 19, fill = NA, stroke = 0.5),
  draw_key = GeomLine$draw_key,
  required_aes = c("y")
)
