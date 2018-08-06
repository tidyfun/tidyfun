# from GGally:
rescale01 <- function(x, xlim=NULL) {
  if (is.null(xlim)) {
    rng <- range(x, na.rm = TRUE)
  } else {
    rng <- xlim
  }
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale11 <- function(x, xlim=NULL) {
  2 * rescale01(x, xlim) - 1
}

#' @export
#' @importFrom ggplot2 ggproto Stat Geom
#' @rdname ggcapellini
#' @usage NULL
#' @format NULL
StatCapellini <- ggproto("StatCapellini", Stat,
  required_aes = c("x", "y", "tf"),
  setup_params = function(data, params) {
    if (is.null(params$arg))
      params$arg <- list(arg(pull(data, tf)))
    if (is.null(params$width)) 
      params$width <- ggplot2::resolution(data$x)
    if (is.null(params$height))
      params$height <- ggplot2::resolution(data$y)
    params
  },
  compute_layer = function(self, data, params, layout) {
    stopifnot(is_tf(pull(data, tf)))
    tf_eval <- 
      suppressMessages(tf_unnest(data, tf, .arg = params$arg, .sep = "___")) %>%
      select(-group) %>% 
      rename(group = tf___id, arg = tf___arg, value = tf___value) %>% 
      mutate(
        xgrid = x, ygrid = y,
        x = x + rescale11(arg) * params$width/2, 
        y = y + rescale11(value) * params$height/2,
        width = params$width,
        height = params$height)
    tf_eval
  },
  # need this so arg, order_by gets recognized as valid parameters
  # because layer() only checks compute_panel & compute_group
  compute_panel = function(self, data, scales, arg, 
      add_lines, add_boxes,  width, height) {
    Stat$compute_panel(self, data, scales)
  }
)


#' @export
#' @rdname ggcapellini
#' @inheritParams ggplot2::stat_identity
#' @param na.rm remove NAs? defaults to `TRUE`
stat_capellini <- function(mapping = NULL, data = NULL, geom = "capellini",
  position = "identity", na.rm = TRUE, show.legend = NA, 
  inherit.aes = TRUE, arg = NULL, add_lines = FALSE, 
  add_boxes = FALSE, width = NULL, height = NULL, ...) {
  layer(
    stat = StatCapellini, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arg = arg, add_lines = add_lines, 
      add_boxes = add_boxes, width = width, height = height,...)
  )
}

# geom --------------------------------------------------------------------

#' @export
#' @rdname ggspaghetti
#' @format NULL
#' @param arg where to evaluate `tf` -- defaults to the default ;)
geom_capellini <- function(mapping = NULL, data = NULL,
  position = "identity", na.rm = TRUE, show.legend = NA, 
  inherit.aes = TRUE, arg = NULL, add_lines = FALSE, add_boxes = FALSE, 
  width = NULL, height = NULL, ...) {
  layer(
    stat = StatCapellini, data = data, mapping = mapping, geom = "capellini", 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arg = arg, add_lines = add_lines, 
      add_boxes = add_boxes, width = width, height = height,...)
  )
}
#' @export
#' @rdname ggspaghetti
#' @usage NULL
#' @format NULL
GeomCapellini <- ggproto("GeomCapellini", Geom,
  setup_data = function(data, params) {
    GeomPath$setup_data(data, params)
    #browser()
  },
  draw_group = function(data, panel_params, coord) {
    browser()
    GeomPath$draw_panel(data, panel_params, coord)
  },
  default_aes = aes(colour = "black", size = 0.5,
    linetype = 1, alpha = 0.5),
  draw_key = GeomPath$draw_key
)

if (FALSE) {
  f <- rgp(25) + 1:25 * 3
  data <- expand.grid(g1 = 1:5, g2 = 1:5)
  data$f <- f
  ggplot(data) + geom_capellini(aes(x = g1, y = g2, tf = f))
}
