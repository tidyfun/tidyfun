#' Error bands using `tf` objects as bounds
#'
#' Plots a shaded ribbon between `tf`-objects `fmax` and `fmin`.
#' Primarily intended to help with plotting confidence bands.
#'
#' @examples
#' set.seed(1221)
#' data <- data.frame(id = factor(1:2))
#' data$f <- tf_rgp(2)
#' data$f_hi <- data$f + 1
#' data$f_lo <- data$f - 1
#' library(ggplot2)
#' ggplot(data, aes(tf = f, color = id)) +
#'   geom_spaghetti() +
#'   geom_papardelle(aes(fmax = f_hi, fmin = f_lo, fill = id)) +
#'   facet_wrap(~id)
#' @name ggpapardelle
NULL

#' @export
#' @importFrom ggplot2 ggproto Stat Geom
#' @rdname ggpapardelle
#' @usage NULL
#' @format NULL
StatPapardelle <- ggproto("StatPapardelle", Stat,
  required_aes = c("fmax", "fmin"),

  compute_panel = function(self, data, scales, arg) {
    stopifnot(is_tf(pull(data, fmax)) & is_tf(pull(data, fmin)))
    if (is.null(arg)) {
      arg <- list(tf_arg(pull(data, fmax)))
    }
    tf_eval <-
      suppressMessages(
        data |>
          mutate(id = names(fmax) %||% seq_along(fmax)) |>
          tf_unnest(c(fmax, fmin), .arg = arg, names_sep = "___")
      ) |>
      select(-group, -fmin___arg) |>
      rename(
        group = id, x = fmax___arg, ymin = fmin___value, ymax = fmax___value
      )
    if (!is.null(scales$x)) {
      tf_eval$x <- tf_eval$x |> scales$x$get_transformation()$transform()
    }
    if (!is.null(scales$y)) {
      tf_eval$ymin <- tf_eval$ymin |> scales$y$get_transformation()$transform()
      tf_eval$ymax <- tf_eval$ymax |> scales$y$get_transformation()$transform()
    }
    tf_eval
  }
)

#' @export
#' @rdname ggpapardelle
#' @inheritParams ggplot2::stat_identity
#' @param na.rm remove NAs? defaults to `TRUE`
stat_papardelle <- function(mapping = NULL, data = NULL, geom = "papardelle",
                           position = "identity", na.rm = TRUE, show.legend = NA,
                           inherit.aes = TRUE, arg = NULL, ...) {
  layer(
    stat = StatPapardelle, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arg = arg, ...)
  )
}

# geom --------------------------------------------------------------------

#' @export
#' @rdname ggpapardelle
#' @format NULL
#' @param arg where to evaluate `fmin` and `fmax` - defaults to `tf_arg(fmax)`
geom_papardelle <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = TRUE, show.legend = NA,
                           inherit.aes = TRUE, arg = NULL, ...) {
  layer(
    stat = StatPapardelle, data = data, mapping = mapping, geom = "papardelle",
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arg = arg, ...)
  )
}
#' @export
#' @rdname ggpapardelle
#' @usage NULL
#' @format NULL
GeomPapardelle <- ggproto("GeomPapardelle", Geom,
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
    fill = "grey70", linetype = 0, alpha = 0.3, linewidth = 0.1
  ),
  draw_key = GeomRibbon$draw_key,
  required_aes = c("ymin", "ymax")
)
