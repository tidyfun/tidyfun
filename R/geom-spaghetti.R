#' Spaghetti plots for `tf` objects
#'
#' Plots a line for each entry of a `tf`-object.
#' `geom_spaghetti` draws a line through each function, and `geom_meatballs`
#' adds points for the evaluated grid values.
#'
#' "Flipped" aesthetics are not implemented for these geoms.
#'
#' @section `y` aesthetic:
#'   Mandatory. Used to designate a column of class `tf` to be visualized.
#' @examples
#' set.seed(1221)
#' data <- data.frame(col = sample(gl(5, 2)))
#' data$f <- tf_rgp(10)
#' data$fi <- tf_jiggle(data$f)
#' data$fb <- tfb(data$f)
#' library(ggplot2)
#' ggplot(data, aes(y = f, color = tf_depth(f))) +
#'   geom_spaghetti()
#' ggplot(data, aes(y = fi, shape = col, color = col)) +
#'   geom_meatballs()
#' ggplot(data, aes(y = fi)) +
#'   geom_meatballs(spaghetti = FALSE) +
#'   facet_wrap(~col)
#' @returns A [ggplot2::layer()] object for use in a ggplot.
#' @name ggspaghetti
#' @family tidyfun visualization
#' @import ggplot2
#' @seealso [geom_cappelini()] for glyph plots, [gglasagna()] for heatmaps.
NULL

# unlist() because tf_mv evaluations are data.frames, not numeric vectors
#' @export
is.finite.tf <- function(x) {
  map_lgl(tf_evaluations(x), \(x) {
    x <- unlist(x)
    all(is.finite(x) | !is.na(x))
  })
}

# needed so ggplot2's find_scale() does not die in vctrs::vec_math() for the
# (atomic, index-backed) tf_mv class before the informative checks below run
#' @export
is.infinite.tf <- function(x) {
  map_lgl(tf_evaluations(x), \(x) any(is.infinite(unlist(x))))
}

# Abort informatively when a multivariate tf_mv lands in a univariate-only
# display. `what` labels the offending function(s) in the error message.
check_tf_1d <- function(x, what, call = rlang::caller_env()) {
  if (is_tf(x) && !tf::is_tf_1d(x)) {
    cli::cli_abort(
      c(
        paste0(what, " does not support multivariate {.cls tf_mv} data."),
        "i" = "Use {.fn tf_ggplot} with {.code aes(tf = ...)} and
               {.code type = \"facet\"} or {.code type = \"trajectory\"},
               or {.fn autoplot}, to display {.cls tf_mv} columns.",
        "i" = "Or extract single components with {.fn tf_component} first."
      ),
      call = call
    )
  }
  invisible(x)
}

#' @export
scale_type.tf <- function(x) "identity"

#' @export
#' @rdname ggspaghetti
#' @family tidyfun visualization
#' @usage NULL
#' @format NULL
StatTf <- ggproto(
  "StatTf",
  Stat,
  required_aes = "y",
  setup_params = function(data, params) {
    # here and not (only) in compute_layer: this must abort before
    # tf_arg() below chokes on multivariate input
    if ("y" %in% names(data)) {
      check_tf_1d(pull(data, y), "{.fn geom_spaghetti}/{.fn geom_meatballs}")
    }
    if (is.null(params$arg)) {
      params$arg <- list(tf_arg(pull(data, y)))
    }
    params
  },
  compute_layer = function(self, data, params, layout) {
    if (!is_tf(data$y)) {
      cli::cli_abort(
        "{.arg y} must be a {.cls tf} object, not {.obj_type_friendly {data$y}}."
      )
    }
    check_tf_1d(data$y, "{.fn geom_spaghetti}/{.fn geom_meatballs}")
    tf_eval <- suppressMessages(
      data |>
        mutate(y____id = names(y) %||% seq_along(y)) |>
        tf_unnest(y, arg = params$arg, names_sep = "____")
    ) |>
      select(-group) |>
      rename(group = y____id, x = y____arg, y = y____value)
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
stat_tf <- function(
  mapping = NULL,
  data = NULL,
  geom = "spaghetti",
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  arg = NULL,
  ...
) {
  ggplot2::layer(
    stat = StatTf,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arg = arg, ...)
  )
}

# geom --------------------------------------------------------------------

#' @export
#' @rdname ggspaghetti
#' @family tidyfun visualization
#' @format NULL
#' @param arg where to evaluate the functions in `y`; defaults to the object's
#'   default evaluation grid.
geom_spaghetti <- function(
  mapping = NULL,
  data = NULL,
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  arg = NULL,
  ...
) {
  ggplot2::layer(
    stat = StatTf,
    data = data,
    mapping = mapping,
    geom = "spaghetti",
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arg = arg, ...)
  )
}
#' @export
#' @rdname ggspaghetti
#' @family tidyfun visualization
#' @usage NULL
#' @format NULL
GeomSpaghetti <- ggplot2::ggproto(
  "GeomSpaghetti",
  ggplot2::Geom,
  default_aes = ggplot2::aes(
    colour = "black",
    linewidth = 0.5,
    linetype = 1,
    alpha = 0.5
  ),
  draw_group = function(data, panel_params, coord) {
    GeomLine$draw_panel(data, panel_params, coord)
  },
  draw_key = GeomLine$draw_key,
  required_aes = c("x", "y", "group"),
  setup_data = function(data, params) {
    GeomLine$setup_data(data, params)
  },
  setup_params = function(data, params) {
    # TODO: implement proper "orientation" - see extending ggplot vignette
    params$flipped_aes <- FALSE
    params
  }
)

#-------------------------------------------------------------------------------

#' @export
#' @rdname ggspaghetti
#' @family tidyfun visualization
#' @format NULL
#' @importFrom grid gList
#' @param spaghetti plot noodles along with meatballs? defaults to TRUE.
geom_meatballs <- function(
  mapping = NULL,
  data = NULL,
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  arg = NULL,
  spaghetti = TRUE,
  ...
) {
  ggplot2::layer(
    stat = StatTf,
    data = data,
    mapping = mapping,
    geom = "meatballs",
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arg = arg, spaghetti = spaghetti, ...)
  )
}
#' @export
#' @rdname ggspaghetti
#' @family tidyfun visualization
#' @usage NULL
#' @format NULL
GeomMeatballs <- ggplot2::ggproto(
  "GeomMeatballs",
  ggplot2::Geom,
  default_aes = ggplot2::aes(
    colour = "black",
    linewidth = 0.5,
    size = 0.5,
    linetype = 1,
    alpha = 0.5,
    shape = 19,
    fill = NA,
    stroke = 0.5
  ),
  draw_group = function(data, panel_params, coord, spaghetti = TRUE) {
    grid::gList(
      if (spaghetti) GeomLine$draw_panel(data, panel_params, coord),
      GeomPoint$draw_panel(data, panel_params, coord)
    )
  },
  draw_key = function(data, params, size) {
    grid::grobTree(
      if (params$spaghetti) draw_key_path(data, params, size),
      draw_key_point(data, params, size)
    )
  },
  required_aes = c("x", "y", "group"),
  setup_params = function(data, params) {
    # TODO: implement proper "orientation" - see extending ggplot vignette
    params$flipped_aes <- FALSE
    params
  },
  setup_data = function(data, params) {
    GeomLine$setup_data(data, params)
  }
)
