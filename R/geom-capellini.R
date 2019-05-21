#' Glyph plots for `tf` objects
#'
#' Plots a miniature glyph or sparkline for each entry of a `tf`-object.
#' (Capellini are tiny little spaghetti noodles.) Aesthetics `x` and `y`
#' specify the location of the glyphs, the `tf` aesthetic defines their shapes.
#' I realize the naming is weird -- `geom_cappelini`,  `geom_cappellini` and
#' `geom_capelini` will work too...
#'
#' @section `tf` aesthetic:
#'   Mandatory. Used to designate a column of class `tf` to be visualized as glyphs.
#' @examples
#' \dontrun{ # takes a little too long for CRAN
#' library(ggplot2)
#' library(tidyverse)
#' weather <- fda::CanadianWeather
#' canada <- data.frame(
#'   place = weather$place,
#'   region = weather$region,
#'   lat = weather$coordinates[,1],
#'   lon = -weather$coordinates[,2],
#'   region = weather$region)
#' canada$temp <- tfd(t(weather$dailyAv[,,1]), arg = 1:365)
#' canada$precipl10 <- tfd(t(weather$dailyAv[,,3]), arg = 1:365) %>% tf_smooth
#' canada_map <-
#'   data.frame(maps::map("world", "Canada", plot = FALSE)[c("x", "y")])
#' # map of canada with annual temperature averages in red, precipitation in blue:
#' ggplot(canada, aes(x = lon, y = lat)) +
#'   geom_capellini(aes(tf = precipl10), width = 3, height = 5, colour = "blue") +
#'   geom_capellini(aes(tf = temp), width = 3, height = 5, colour = "red") +
#'   geom_path(data = canada_map, aes(x = x, y = y), alpha = .1) +
#'   coord_quickmap()
#'
#' ggplot(canada, aes(x = lon, y = lat, colour = region)) +
#'   geom_capellini(aes(tf = precipl10), width = 5, height = 3,
#'     line.linetype = 1, box.fill = "white", box.alpha=.5, box.colour = NA)
#' }
#' @name ggcapellini
#' @seealso GGally::glyphs
NULL

#' @export
#' @importFrom ggplot2 ggproto Stat Geom
#' @importFrom GGally rescale11 rescale01
#' @rdname ggcapellini
#' @usage NULL
#' @format NULL
StatCapellini <- ggproto("StatCapellini", Stat,
  required_aes = c("x", "y", "tf"),
  setup_params = function(data, params) {
    if (is.null(params$arg)) {
      params$arg <- list(tf_arg(pull(data, tf)))
    }
    if (is.null(params$width)) {
      params$width <- ggplot2::resolution(data$x) / 1.5
    }
    if (is.null(params$height)) {
      params$height <- ggplot2::resolution(data$y) / 1.5
    }
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
        x = x + rescale11(arg) * params$width / 2,
        y = y + rescale11(value) * params$height / 2,
        width = params$width,
        height = params$height,
        lines = params$add_lines,
        boxes = params$add_boxes
      )
    tf_eval
  },
  # need this so arg etc get recognized as valid parameters
  # because layer() only checks compute_panel & compute_group
  compute_panel = function(self, data, scales, arg,
                             add_lines, add_boxes, width, height) {
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
                           add_boxes = TRUE, width = NULL, height = NULL, ...) {
  layer(
    stat = StatCapellini, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm, arg = arg, add_lines = add_lines,
      add_boxes = add_boxes, width = width, height = height, ...
    )
  )
}

# geom --------------------------------------------------------------------

#' @export
#' @rdname ggcapellini
#' @format NULL
#' @param stat that's "capellini"!
#' @param arg where to evaluate `tf` -- defaults to the default ;)
#' @param add_lines should a reference line in the middle of the range of the
#'    functions' values be added to each glyph? defaults to TRUE
#' @param add_boxes should a box be added to frame each glyph? defaults to TRUE
#' @param width the width of the glyphs. Defaults to 2/3 of the [ggplot2::resolution()]
#'   of the variable for the `x`-aesthetic, this will be too small if any values
#'   are close together.
#' @param height the height of the glyphs. Defaults to 2/3 of the [ggplot2::resolution()]
#'   of the variable for the `y`-aesthetic, this will be too small if any values
#'   are close together.
#' @param box.colour aesthetic property of the box
#' @param box.linetype  aesthetic property of the box
#' @param box.fill  aesthetic property of the box
#' @param box.size  aesthetic property of the box
#' @param box.alpha  aesthetic property of the box
#' @param line.colour aesthetic property of the reference line
#' @param line.linetype aesthetic property of the reference line
#' @param line.size aesthetic property of of the reference line
#' @param line.alpha aesthetic property of the reference line
geom_capellini <- 
  function(mapping = NULL, data = NULL, stat = "capellini",
           position = "identity", ..., na.rm = TRUE, show.legend = NA,
           inherit.aes = TRUE, arg = NULL, add_lines = TRUE, add_boxes = TRUE,
           width = NULL, height = NULL, box.colour = "#0000001A", 
           box.linetype = 1, box.fill = NA, box.size = .1, box.alpha = .1,
           line.colour = "black", line.linetype = 2, line.size = .3, 
           line.alpha = .5) {
  layer(
    stat = StatCapellini, data = data, mapping = mapping, geom = "capellini",
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm, arg = arg, add_lines = add_lines,
      add_boxes = add_boxes, width = width, height = height,
      box.colour = box.colour, box.linetype = box.linetype,
      box.fill = box.fill, box.size = box.size, box.alpha = box.alpha,
      line.colour = line.colour, line.linetype = line.linetype,
      line.size = line.size, line.alpha = line.alpha, ...
    )
  )
}
#' @export
#' @rdname ggcapellini
#' @usage NULL
geom_cappellini <- geom_capellini
#' @export
#' @usage NULL
#' @rdname ggcapellini
geom_capelini <- geom_capellini
#' @export
#' @usage NULL
#' @rdname ggcapellini
geom_cappelini <- geom_capellini


#' @export
#' @rdname ggcapellini
#' @usage NULL
#' @format NULL
GeomCapellini <- ggproto("GeomCapellini", Geom,
  setup_data = function(data, params) {
    GeomPath$setup_data(data, params)
  },
  draw_group = function(data, panel_params, coord,
                          box.colour = "#0000001A", box.linetype = 1,
                          box.fill = NA, box.size = .1, box.alpha = .1,
                          line.colour = "black", line.linetype = 2,
                          line.size = .3, line.alpha = .5) {
    glyph_grob <- GeomPath$draw_panel(data, panel_params, coord)
    if (data$lines[1]) {
      lines <- data.frame(
        x = data$xgrid[1] - data$width[1] / 2,
        xend = data$xgrid[1] + data$width[1] / 2,
        y = data$ygrid[1],
        yend = data$ygrid[1],
        colour = line.colour,
        linetype = line.linetype,
        size = line.size,
        alpha = line.alpha
      )
      lines_grob <- GeomSegment$draw_panel(lines, panel_params, coord)
    } else {
      lines_grob <- NULL
    }
    if (data$boxes[1]) {
      boxes <- data.frame(
        xmin = data$xgrid[1] - data$width[1] / 2,
        xmax = data$xgrid[1] + data$width[1] / 2,
        ymin = data$ygrid[1] - data$height[1] / 2,
        ymax = data$ygrid[1] + data$height[1] / 2,
        colour = box.colour, # scales::alpha("black", .1),
        linetype = box.linetype,
        fill = box.fill,
        alpha = box.alpha,
        size = box.size
      )
      boxes_grob <- GeomRect$draw_panel(boxes, panel_params, coord)
    } else {
      boxes_grob <- NULL
    }
    grid::gList(
      lines_grob,
      boxes_grob,
      glyph_grob
    )
  },
  default_aes = aes(
    colour = "black", size = 0.5,
    linetype = 1, alpha = 0.5
  ),
  draw_key = GeomPath$draw_key
)
