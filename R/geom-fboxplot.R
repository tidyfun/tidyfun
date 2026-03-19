#' Functional boxplots for `tf` objects
#'
#' Draw functional boxplots based on functional depth rankings.
#'
#' `stat_fboxplot()` computes a median curve (thick line), a central region
#' envelope (filled ribbon), functional fences (dashed lines), and optional
#' outlying curves (defined as "exceeds the fences somewhere", plotted as solid lines)
#' from a `tf` column. `geom_fboxplot()` draws these summaries as a band plus
#' line layers.
#'
#' The interface intentionally follows [ggplot2::stat_boxplot()] and
#' [ggplot2::geom_boxplot()] where this is meaningful for functional data.
#' Use `aes(tf = f)` to map a `tf` column. Separate functional boxplots inside a
#' panel are defined by `group`, `colour`/`color`, or `fill`; separate panels are
#' handled through facetting. Note that only `color` or `fill` need to be
#' specified explicitly, as the other will be automatically mapped to the same
#' variable if not provided.
#'
#' @param mapping,data,position,show.legend,inherit.aes,na.rm,orientation,...
#'   Passed on to [ggplot2::layer()].
#' @param stat,geom Use the functional boxplot stat/geom.
#' @param coef Inflation factor for the central envelope used to define outer fences, defaults to 1.5.
#' @param depth Character scalar naming the built-in depth measure passed to
#'   [tf::tf_depth()] when `depth_fn` is `NULL`.
#' @param depth_fn Optional custom depth function. Must return one numeric depth
#'   value per function.
#' @param fence_fn Optional custom fence function. Must return a list with
#'   elements `lower`, `upper`, and `outliers`.
#' @param central Fraction of deepest curves used to construct the central
#'   envelope. Defaults to `0.5`.
#' @param arg Optional evaluation grid used for depth calculation, envelopes, and
#'   drawing. Defaults to the natural grid of the mapped `tf`.
#' @param outliers Should outlying curves be drawn?
#' @param outlier.colour,outlier.color,outlier.fill,outlier.shape,outlier.size
#'   Styling parameters for outlier curves.
#' @param outlier.stroke Accepted for interface compatibility with
#'   [ggplot2::geom_boxplot()] but ignored because functional outliers are drawn
#'   as curves.
#' @param outlier.alpha Alpha used for outlier curves.
#' @param whisker.colour,whisker.color,whisker.linetype,whisker.linewidth Styling
#'   parameters for fence lines.
#' @param staple.colour,staple.color,staple.linetype,staple.linewidth Accepted for
#'   interface compatibility with [ggplot2::geom_boxplot()] but currently unused.
#' @param median.colour,median.color,median.linetype,median.linewidth Styling
#'   parameters for the median curve.
#' @param box.colour,box.color,box.linetype,box.linewidth Styling parameters for
#'   the central band outline.
#' @param notch,notchwidth,staplewidth,varwidth Accepted for interface
#'   compatibility with [ggplot2::geom_boxplot()] but currently unused.
#' @returns A ggplot2 layer.
#' @examples
#' \donttest{
#' library(ggplot2)
#' set.seed(1312)
#' data <- data.frame(id = 1:50, grp = rep(c("A", "B"), each = 25))
#' data$f <- tf_rgp(50) + 5 * (data$grp == "A")
#'
#' tf_ggplot(data, aes(tf = f, fill = grp)) + # same as `colour = grp` here!
#'   geom_fboxplot(alpha = 0.3)
#'
#' tf_ggplot(data, aes(tf = f)) +
#'   geom_fboxplot(orientation = "y")
#' }
#' @name ggfboxplot
NULL

normalize_fboxplot_mapping <- function(mapping) {
  mapping <- mapping %||% aes()
  use_group_aes <- "group" %in% names(mapping)

  xy_aes <- intersect(c("x", "y"), names(mapping))
  if (length(xy_aes) > 0) {
    if (any(c("colour", "color", "fill") %in% names(mapping))) {
      cli::cli_abort(c(
        "{.fn geom_fboxplot}/{.fn stat_fboxplot} do not use {.code x}/{.code y} for grouping",
        "i" = "Use {.code aes(tf = f, colour = grp, fill = grp)} and/or facets instead."
      ))
    }

    group_expr <- mapping[[xy_aes[1]]]
    mapping$group <- group_expr
    mapping$fill <- group_expr
    mapping$colour <- group_expr
    mapping[xy_aes] <- NULL
    use_group_aes <- TRUE

    cli::cli_warn(c(
      "{.fn geom_fboxplot}/{.fn stat_fboxplot} ignore {.code x}/{.code y} grouping semantics",
      "i" = "Mapped {.code {xy_aes[1]}} to {.code group}, {.code fill}, and {.code colour} instead."
    ))
  }

  list(mapping = mapping, use_group_aes = use_group_aes)
}

resolve_fboxplot_orientation <- function(orientation) {
  if (is.na(orientation) || is.null(orientation)) {
    return("x")
  }

  orientation <- match.arg(orientation, c("x", "y"))
  orientation
}

build_fboxplot_groups <- function(data, use_group_aes = FALSE) {
  keys <- list()

  if (isTRUE(use_group_aes) && "group" %in% names(data)) {
    keys[["group"]] <- data$group
  }
  if ("colour" %in% names(data)) {
    keys[["colour"]] <- data$colour
  }
  if ("fill" %in% names(data)) {
    keys[["fill"]] <- data$fill
  }

  keys <- Filter(\(x) !all(is.na(x)), keys)
  if (length(keys) == 0) {
    return(factor(rep("all", nrow(data))))
  }

  do.call(interaction, c(keys, list(drop = TRUE, lex.order = TRUE)))
}

compute_fboxplot_depth <- function(x, arg, depth, depth_fn = NULL) {
  depth_values <- if (is.null(depth_fn)) {
    tf::tf_depth(x, arg = arg, depth = depth)
  } else {
    depth_fn(x, arg = arg)
  }

  if (!is.numeric(depth_values) || length(depth_values) != length(x)) {
    cli::cli_abort(c(
      "{.arg depth_fn} must return one numeric value per function",
      "i" = "Expected {length(x)} values, got {length(depth_values)}."
    ))
  }

  depth_values
}

default_fboxplot_arg <- function(tf_vec) {
  tf_arg0 <- tf_arg(tf_vec)
  if (!is.list(tf_arg0)) {
    return(tf_arg0)
  }

  arg_starts <- vapply(tf_arg0, min, numeric(1))
  arg_ends <- vapply(tf_arg0, max, numeric(1))
  common_lower <- max(arg_starts)
  common_upper <- min(arg_ends)

  if (common_lower > common_upper) {
    cli::cli_abort(c(
      "Cannot infer a default evaluation grid for irregular {.cls tf} data with no common support.",
      "i" = "Provide {.arg arg} explicitly."
    ))
  }

  common_arg <- sort(unique(unlist(tf_arg0)))
  common_arg <- common_arg[
    common_arg >= common_lower & common_arg <= common_upper
  ]

  if (length(common_arg) == 0) {
    if (identical(common_lower, common_upper)) {
      return(common_lower)
    }

    cli::cli_abort(c(
      "Cannot infer a default evaluation grid for irregular {.cls tf} data.",
      "i" = "Provide {.arg arg} explicitly."
    ))
  }

  common_arg
}

prepare_fboxplot_curves <- function(tf_vec, data, arg) {
  curve_mat <- as.matrix(tf_vec, arg = arg, interpolate = TRUE)
  keep <- stats::complete.cases(curve_mat)

  if (!all(keep)) {
    cli::cli_warn(c(
      "Dropped {sum(!keep)} function{?s} from {.fn stat_fboxplot} because they cannot be evaluated on the selected grid.",
      "i" = "Provide {.arg arg} explicitly to control the evaluation support."
    ))
  }

  if (!any(keep)) {
    return(NULL)
  }

  list(
    tf_vec = tf_vec[keep],
    data = data[keep, , drop = FALSE],
    curve_mat = curve_mat[keep, , drop = FALSE]
  )
}

as_fboxplot_curve <- function(x, arg, what) {
  if (is_tf(x)) {
    curve_mat <- as.matrix(x, arg = arg, interpolate = TRUE)
    if (nrow(curve_mat) != 1) {
      cli::cli_abort("{.arg {what}} must evaluate to exactly one function.")
    }
    return(as.numeric(curve_mat[1, ]))
  }

  if (is.numeric(x) && length(x) == length(arg)) {
    return(as.numeric(x))
  }

  cli::cli_abort(c(
    "{.arg {what}} must be a numeric vector with one value per evaluation point or a length-1 {.cls tf}.",
    "i" = "Expected length {length(arg)}, got {.obj_type_friendly {x}}."
  ))
}

default_fboxplot_fence <- function(
  tf_vec,
  arg,
  depth,
  median,
  central_lower,
  central_upper,
  coef,
  central
) {
  curve_mat <- as.matrix(tf_vec, arg = arg, interpolate = TRUE)
  spread <- central_upper - central_lower
  lower <- central_lower - coef * spread
  upper <- central_upper + coef * spread
  outliers <- apply(
    curve_mat < rep(lower, each = nrow(curve_mat)) |
      curve_mat > rep(upper, each = nrow(curve_mat)),
    1,
    any,
    na.rm = TRUE
  )

  list(lower = lower, upper = upper, outliers = outliers)
}

compute_fboxplot_fence <- function(
  tf_vec,
  arg,
  depth,
  median,
  central_lower,
  central_upper,
  coef,
  central,
  fence_fn = NULL
) {
  fence <- if (is.null(fence_fn)) {
    default_fboxplot_fence(
      tf_vec = tf_vec,
      arg = arg,
      depth = depth,
      median = median,
      central_lower = central_lower,
      central_upper = central_upper,
      coef = coef,
      central = central
    )
  } else {
    fence_fn(
      tf_vec = tf_vec,
      arg = arg,
      depth = depth,
      median = median,
      central_lower = central_lower,
      central_upper = central_upper,
      coef = coef,
      central = central
    )
  }

  if (
    !is.list(fence) || !all(c("lower", "upper", "outliers") %in% names(fence))
  ) {
    cli::cli_abort(
      "{.arg fence_fn} must return a list with components {.code lower}, {.code upper}, and {.code outliers}."
    )
  }

  lower <- as_fboxplot_curve(fence$lower, arg = arg, what = "lower")
  upper <- as_fboxplot_curve(fence$upper, arg = arg, what = "upper")
  outliers <- fence$outliers

  if (!is.logical(outliers) || length(outliers) != length(tf_vec)) {
    cli::cli_abort(c(
      "{.arg fence_fn} must return one logical outlier flag per function",
      "i" = "Expected {length(tf_vec)} values, got {length(outliers)}."
    ))
  }

  list(lower = lower, upper = upper, outliers = outliers)
}

curve_df <- function(x, y, component, stat_group, path_group, orientation) {
  if (orientation == "x") {
    data.frame(
      x = x,
      y = y,
      .fbox_component = component,
      .fbox_stat_group = stat_group,
      group = path_group
    )
  } else {
    data.frame(
      x = y,
      y = x,
      .fbox_component = component,
      .fbox_stat_group = stat_group,
      group = path_group
    )
  }
}

band_df <- function(x, lower, upper, stat_group, orientation) {
  if (orientation == "x") {
    data.frame(
      x = x,
      ymin = lower,
      ymax = upper,
      .fbox_component = "box",
      .fbox_stat_group = stat_group,
      group = stat_group
    )
  } else {
    data.frame(
      y = x,
      xmin = lower,
      xmax = upper,
      .fbox_component = "box",
      .fbox_stat_group = stat_group,
      group = stat_group
    )
  }
}

make_box_polygon <- function(data) {
  if ("ymin" %in% names(data)) {
    data <- data[order(data$x), ]
    data.frame(
      x = c(data$x, rev(data$x)),
      y = c(data$ymin, rev(data$ymax)),
      group = data$group[1]
    )
  } else {
    data <- data[order(data$y), ]
    data.frame(
      x = c(data$xmin, rev(data$xmax)),
      y = c(data$y, rev(data$y)),
      group = data$group[1]
    )
  }
}

prepare_component_defaults <- function(
  data,
  colour = NULL,
  fill = NULL,
  linewidth = NULL,
  linetype = NULL,
  alpha = NULL
) {
  if (!is.null(colour)) data$colour <- colour
  if (!is.null(fill)) data$fill <- fill
  if (!is.null(linewidth)) data$linewidth <- linewidth
  if (!is.null(linetype)) data$linetype <- linetype
  if (!is.null(alpha)) data$alpha <- alpha
  data
}

default_fboxplot_colour <- function(data) {
  colour_vals <- unique(stats::na.omit(data$colour))
  fill_vals <- unique(stats::na.omit(data$fill))
  default_colour <- as.character(GeomFboxplot$default_aes$colour)

  if (
    length(colour_vals) > 0 &&
      (colour_vals[1] != default_colour || length(fill_vals) == 0)
  ) {
    return(colour_vals[1])
  }
  if (length(fill_vals) > 0) {
    return(fill_vals[1])
  }
  if (length(colour_vals) > 0) {
    return(colour_vals[1])
  }

  default_colour
}

default_fboxplot_fill <- function(data, fallback_colour = NULL) {
  default_fill <- as.character(GeomFboxplot$default_aes$fill)
  fill_vals <- unique(stats::na.omit(data$fill))

  if (length(fill_vals) > 0 && any(fill_vals != default_fill)) {
    return(fill_vals[1])
  }

  fallback_colour %||% as.character(GeomFboxplot$default_aes$fill)
}

has_user_fboxplot_colour <- function(data) {
  default_colour <- as.character(GeomFboxplot$default_aes$colour)
  default_fill <- as.character(GeomFboxplot$default_aes$fill)
  colour_vals <- unique(stats::na.omit(data$colour))
  fill_vals <- unique(stats::na.omit(data$fill))

  any(colour_vals != default_colour) || any(fill_vals != default_fill)
}

canonicalise_fboxplot_colour_args <- function(args) {
  for (prefix in c("outlier", "whisker", "staple", "median", "box")) {
    color_name <- paste0(prefix, ".color")
    colour_name <- paste0(prefix, ".colour")
    colour_value <- args[[color_name]] %||% args[[colour_name]]
    args[[color_name]] <- NULL
    args[[colour_name]] <- colour_value
  }
  args
}

get_fboxplot_scale_transform <- function(scales, orientation) {
  scale_obj <- if (orientation == "x") scales$y else scales$x
  if (is.null(scale_obj)) {
    return(NULL)
  }

  trans <- scale_obj$trans %||% scale_obj$scale$trans
  transform <- trans$transform %||% trans$transform_df

  if (is.null(transform)) {
    return(NULL)
  }

  transform
}

apply_fboxplot_scale_transform <- function(data, scales, orientation) {
  transform <- get_fboxplot_scale_transform(scales, orientation)
  if (is.null(transform)) {
    return(data)
  }

  cols <- if (orientation == "x") {
    intersect(c("y", "ymin", "ymax"), names(data))
  } else {
    intersect(c("x", "xmin", "xmax"), names(data))
  }

  for (col in cols) {
    data[[col]] <- transform(data[[col]])
  }

  data
}

warn_unsupported_fboxplot_params <- function(
  staplewidth,
  staple.colour,
  staple.color,
  staple.linetype,
  staple.linewidth,
  notch,
  notchwidth,
  varwidth,
  outlier.shape,
  outlier.stroke
) {
  if (
    !identical(staplewidth, 0) ||
      !is.null(staple.colour) ||
      !is.null(staple.color) ||
      !is.null(staple.linetype) ||
      !is.null(staple.linewidth)
  ) {
    cli::cli_warn(
      "{.arg staple*} parameters are accepted for compatibility but currently ignored by {.fn geom_fboxplot}."
    )
  }
  if (!identical(notch, FALSE) || !identical(notchwidth, 0.5)) {
    cli::cli_warn(
      "{.arg notch} and {.arg notchwidth} are accepted for compatibility but currently ignored by {.fn geom_fboxplot}."
    )
  }
  if (!identical(varwidth, FALSE)) {
    cli::cli_warn(
      "{.arg varwidth} is accepted for compatibility but currently ignored by {.fn geom_fboxplot}."
    )
  }
  if (!is.null(outlier.shape) || !identical(outlier.stroke, 0.5)) {
    cli::cli_warn(
      "{.arg outlier.shape} and {.arg outlier.stroke} are accepted for compatibility but ignored because functional outliers are drawn as curves."
    )
  }
}

#' @export
#' @rdname ggfboxplot
#' @usage NULL
#' @format NULL
StatFboxplot <- ggplot2::ggproto(
  "StatFboxplot",
  ggplot2::Stat,
  required_aes = c("tf"),
  retransform = FALSE,
  setup_params = function(data, params) {
    params$orientation <- resolve_fboxplot_orientation(params$orientation)
    params
  },
  compute_panel = function(
    data,
    scales,
    coef = 1.5,
    na.rm = FALSE,
    orientation = "x",
    depth = "MBD",
    depth_fn = NULL,
    fence_fn = NULL,
    central = 0.5,
    arg = NULL,
    use_group_aes = FALSE
  ) {
    if (!"tf" %in% names(data) || !is_tf(data$tf)) {
      cli::cli_abort(
        "{.fn stat_fboxplot} requires a {.code tf} aesthetic mapping to a {.cls tf} object."
      )
    }

    if (
      !is.numeric(central) ||
        length(central) != 1 ||
        is.na(central) ||
        central <= 0 ||
        central > 1
    ) {
      cli::cli_abort(
        "{.arg central} must be a single number in {.code (0, 1]}."
      )
    }

    tf_vec <- data$tf
    if (isTRUE(na.rm)) {
      keep <- !is.na(tf_vec)
      data <- data[keep, , drop = FALSE]
      tf_vec <- tf_vec[keep]
    }

    if (nrow(data) == 0) {
      return(data.frame())
    }

    arg <- arg %||% default_fboxplot_arg(tf_vec)

    stat_groups <- build_fboxplot_groups(data, use_group_aes = use_group_aes)
    pieces <- vector("list", length(levels(stat_groups)))

    for (idx in seq_along(levels(stat_groups))) {
      level <- levels(stat_groups)[idx]
      take <- stat_groups == level
      group_data <- data[take, , drop = FALSE]
      group_tf <- group_data$tf

      prepared <- prepare_fboxplot_curves(
        tf_vec = group_tf,
        data = group_data,
        arg = arg
      )
      if (is.null(prepared)) {
        next
      }

      group_tf <- prepared$tf_vec
      group_data <- prepared$data
      curve_mat <- prepared$curve_mat

      depth_values <- compute_fboxplot_depth(
        x = group_tf,
        arg = arg,
        depth = depth,
        depth_fn = depth_fn
      )

      rank_idx <- order(depth_values, decreasing = TRUE, na.last = NA)
      if (length(rank_idx) == 0) {
        next
      }

      n_central <- max(1L, ceiling(length(rank_idx) * central))
      central_idx <- rank_idx[seq_len(n_central)]
      median_idx <- rank_idx[1]

      central_mat <- curve_mat[central_idx, , drop = FALSE]
      central_lower <- apply(central_mat, 2, min, na.rm = TRUE)
      central_upper <- apply(central_mat, 2, max, na.rm = TRUE)
      median_curve <- as.numeric(curve_mat[median_idx, ])

      fence <- compute_fboxplot_fence(
        tf_vec = group_tf,
        arg = arg,
        depth = depth_values,
        median = median_curve,
        central_lower = central_lower,
        central_upper = central_upper,
        coef = coef,
        central = central,
        fence_fn = fence_fn
      )

      exemplar <- group_data[rep(1, length(arg)), , drop = FALSE]
      exemplar$tf <- NULL
      exemplar$group <- NULL

      next_group <- idx * 100000L
      box_piece <- cbind(
        band_df(
          x = arg,
          lower = central_lower,
          upper = central_upper,
          stat_group = idx,
          orientation = orientation
        ),
        exemplar
      )
      median_piece <- cbind(
        curve_df(
          x = arg,
          y = median_curve,
          component = "median",
          stat_group = idx,
          path_group = next_group + 1L,
          orientation = orientation
        ),
        exemplar
      )
      whisker_lower_piece <- cbind(
        curve_df(
          x = arg,
          y = fence$lower,
          component = "whisker_lower",
          stat_group = idx,
          path_group = next_group + 2L,
          orientation = orientation
        ),
        exemplar
      )
      whisker_upper_piece <- cbind(
        curve_df(
          x = arg,
          y = fence$upper,
          component = "whisker_upper",
          stat_group = idx,
          path_group = next_group + 3L,
          orientation = orientation
        ),
        exemplar
      )

      outlier_idx <- which(fence$outliers)
      outlier_piece <- if (length(outlier_idx) > 0) {
        outlier_frames <- vector("list", length(outlier_idx))
        for (j in seq_along(outlier_idx)) {
          row_id <- outlier_idx[j]
          curve_data <- cbind(
            curve_df(
              x = arg,
              y = as.numeric(curve_mat[row_id, ]),
              component = "outlier",
              stat_group = idx,
              path_group = next_group + 3L + j,
              orientation = orientation
            ),
            exemplar
          )
          outlier_frames[[j]] <- curve_data
        }
        dplyr::bind_rows(outlier_frames)
      } else {
        NULL
      }

      pieces[[idx]] <- dplyr::bind_rows(
        box_piece,
        median_piece,
        whisker_lower_piece,
        whisker_upper_piece,
        outlier_piece
      )
    }

    pieces <- dplyr::bind_rows(pieces)
    apply_fboxplot_scale_transform(
      pieces,
      scales = scales,
      orientation = orientation
    )
  }
)

#' @export
#' @rdname ggfboxplot
stat_fboxplot <- function(
  mapping = NULL,
  data = NULL,
  geom = "fboxplot",
  position = "identity",
  ...,
  orientation = NA,
  coef = 1.5,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  depth = "MBD",
  depth_fn = NULL,
  fence_fn = NULL,
  central = 0.5,
  arg = NULL
) {
  normalized <- normalize_fboxplot_mapping(mapping)

  ggplot2::layer(
    stat = StatFboxplot,
    data = data,
    mapping = normalized$mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      orientation = orientation,
      coef = coef,
      na.rm = na.rm,
      depth = depth,
      depth_fn = depth_fn,
      fence_fn = fence_fn,
      central = central,
      arg = arg,
      use_group_aes = normalized$use_group_aes,
      ...
    )
  )
}

#' @export
#' @rdname ggfboxplot
#' @usage NULL
#' @format NULL
GeomFboxplot <- ggplot2::ggproto(
  "GeomFboxplot",
  ggplot2::Geom,
  required_aes = character(0),
  optional_aes = c("tf", "tf_x", "tf_y", "tf_ymin", "tf_ymax"),
  default_aes = ggplot2::aes(
    colour = "#333333",
    fill = "grey80",
    linewidth = 0.5,
    linetype = 1,
    alpha = 0.35
  ),
  draw_panel = function(
    self,
    data,
    panel_params,
    coord,
    outliers = TRUE,
    outlier.colour = NULL,
    outlier.color = NULL,
    outlier.fill = NULL,
    outlier.shape = NULL,
    outlier.size = NULL,
    outlier.stroke = 0.5,
    outlier.alpha = NULL,
    whisker.colour = NULL,
    whisker.color = NULL,
    whisker.linetype = NULL,
    whisker.linewidth = NULL,
    staple.colour = NULL,
    staple.color = NULL,
    staple.linetype = NULL,
    staple.linewidth = NULL,
    median.colour = NULL,
    median.color = NULL,
    median.linetype = NULL,
    median.linewidth = NULL,
    box.colour = NULL,
    box.color = NULL,
    box.linetype = NULL,
    box.linewidth = NULL,
    notch = FALSE,
    notchwidth = 0.5,
    staplewidth = 0,
    varwidth = FALSE
  ) {
    warn_unsupported_fboxplot_params(
      staplewidth = staplewidth,
      staple.colour = staple.colour,
      staple.color = staple.color,
      staple.linetype = staple.linetype,
      staple.linewidth = staple.linewidth,
      notch = notch,
      notchwidth = notchwidth,
      varwidth = varwidth,
      outlier.shape = outlier.shape,
      outlier.stroke = outlier.stroke
    )

    grobs <- list()
    stat_groups <- unique(data$.fbox_stat_group)

    for (stat_group in stat_groups) {
      stat_data <- data[data$.fbox_stat_group == stat_group, , drop = FALSE]
      group_colour <- default_fboxplot_colour(stat_data)
      group_fill <- default_fboxplot_fill(
        stat_data,
        fallback_colour = group_colour
      )
      whisker_colour <- whisker.color %||% whisker.colour %||% group_colour
      median_colour <- median.color %||%
        median.colour %||%
        if (has_user_fboxplot_colour(stat_data)) group_colour else "black"
      outlier_colour <- outlier.color %||% outlier.colour %||% group_colour
      outlier_linewidth <- outlier.size %||% stat_data$linewidth[1]

      box_data <- stat_data[stat_data$.fbox_component == "box", , drop = FALSE]
      if (nrow(box_data) > 0) {
        box_poly <- make_box_polygon(box_data)
        box_poly <- cbind(
          box_poly,
          data.frame(
            colour = NA,
            fill = default_fboxplot_fill(
              box_data,
              fallback_colour = group_colour
            ),
            linewidth = 0,
            linetype = 0,
            alpha = box_data$alpha[1]
          )
        )
        grobs[[length(grobs) + 1]] <- GeomPolygon$draw_panel(
          box_poly,
          panel_params,
          coord
        )
      }

      whisker_data <- stat_data[
        stat_data$.fbox_component %in% c("whisker_lower", "whisker_upper"),
        ,
        drop = FALSE
      ]
      if (nrow(whisker_data) > 0) {
        whisker_data <- prepare_component_defaults(
          whisker_data,
          colour = whisker_colour,
          linewidth = whisker.linewidth %||% whisker_data$linewidth[1],
          linetype = whisker.linetype %||% 2
        )
        grobs[[length(grobs) + 1]] <- GeomPath$draw_panel(
          whisker_data,
          panel_params,
          coord
        )
      }

      if (isTRUE(outliers)) {
        outlier_data <- stat_data[
          stat_data$.fbox_component == "outlier",
          ,
          drop = FALSE
        ]
        if (nrow(outlier_data) > 0) {
          outlier_data <- prepare_component_defaults(
            outlier_data,
            colour = outlier_colour,
            fill = outlier.fill,
            linewidth = outlier_linewidth,
            alpha = outlier.alpha %||% outlier_data$alpha[1]
          )
          grobs[[length(grobs) + 1]] <- GeomPath$draw_panel(
            outlier_data,
            panel_params,
            coord
          )
        }
      }

      median_data <- stat_data[
        stat_data$.fbox_component == "median",
        ,
        drop = FALSE
      ]
      if (nrow(median_data) > 0) {
        median_data <- prepare_component_defaults(
          median_data,
          colour = median_colour,
          linewidth = median.linewidth %||% (median_data$linewidth[1] * 1.75),
          linetype = median.linetype %||% median_data$linetype[1],
          alpha = 1
        )
        grobs[[length(grobs) + 1]] <- GeomPath$draw_panel(
          median_data,
          panel_params,
          coord
        )
      }
    }

    do.call(grid::grobTree, grobs)
  },
  draw_key = ggplot2::draw_key_polygon
)

#' @export
#' @rdname ggfboxplot
geom_fboxplot <- function(
  mapping = NULL,
  data = NULL,
  stat = "fboxplot",
  position = "identity",
  ...,
  outliers = TRUE,
  outlier.colour = NULL,
  outlier.color = NULL,
  outlier.fill = NULL,
  outlier.shape = NULL,
  outlier.size = NULL,
  outlier.stroke = 0.5,
  outlier.alpha = NULL,
  whisker.colour = NULL,
  whisker.color = NULL,
  whisker.linetype = NULL,
  whisker.linewidth = NULL,
  staple.colour = NULL,
  staple.color = NULL,
  staple.linetype = NULL,
  staple.linewidth = NULL,
  median.colour = NULL,
  median.color = NULL,
  median.linetype = NULL,
  median.linewidth = NULL,
  box.colour = NULL,
  box.color = NULL,
  box.linetype = NULL,
  box.linewidth = NULL,
  notch = FALSE,
  notchwidth = 0.5,
  staplewidth = 0,
  varwidth = FALSE,
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE,
  depth = "MBD",
  depth_fn = NULL,
  fence_fn = NULL,
  central = 0.5,
  arg = NULL
) {
  normalized <- normalize_fboxplot_mapping(mapping)

  params <- canonicalise_fboxplot_colour_args(list(
    outliers = outliers,
    outlier.colour = outlier.colour,
    outlier.color = outlier.color,
    outlier.fill = outlier.fill,
    outlier.shape = outlier.shape,
    outlier.size = outlier.size,
    outlier.stroke = outlier.stroke,
    outlier.alpha = outlier.alpha,
    whisker.colour = whisker.colour,
    whisker.color = whisker.color,
    whisker.linetype = whisker.linetype,
    whisker.linewidth = whisker.linewidth,
    staple.colour = staple.colour,
    staple.color = staple.color,
    staple.linetype = staple.linetype,
    staple.linewidth = staple.linewidth,
    median.colour = median.colour,
    median.color = median.color,
    median.linetype = median.linetype,
    median.linewidth = median.linewidth,
    box.colour = box.colour,
    box.color = box.color,
    box.linetype = box.linetype,
    box.linewidth = box.linewidth,
    notch = notch,
    notchwidth = notchwidth,
    staplewidth = staplewidth,
    varwidth = varwidth,
    na.rm = na.rm,
    orientation = orientation,
    depth = depth,
    depth_fn = depth_fn,
    fence_fn = fence_fn,
    central = central,
    arg = arg,
    use_group_aes = normalized$use_group_aes
  ))

  ggplot2::layer(
    stat = stat,
    data = data,
    mapping = normalized$mapping,
    geom = GeomFboxplot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = c(params, list(...))
  )
}
