#' Create a tf-aware ggplot
#'
#' `tf_ggplot()` creates a ggplot object that can handle tf (functional data) aesthetics.
#' It works similarly to `ggplot()` but automatically transforms tf objects into
#' long-format data suitable for standard ggplot2 geoms.
#'
#' @param data Default dataset to use for plot. If not provided, must be supplied
#'   in each layer added to the plot.
#' @param mapping Default list of aesthetic mappings to use for plot. Can include
#'   tf-specific aesthetics like `tf`, `tf_x`, `tf_y`, `tf_ymin`, `tf_ymax`.
#' @param arg Optional. Evaluation grid for tf objects. A numeric vector of arg
#'   values, or a single integer specifying the desired grid length (resolved to
#'   an equidistant grid over the tf domain). If `NULL` (default), uses the
#'   natural grid of the tf objects.
#' @param interpolate Logical. Should tf objects be interpolated to the evaluation
#'   grid? Defaults to TRUE. In `tf_mv` trajectory plots this is ignored and
#'   interpolation is always used to pair components on a common argument grid.
#' @param type Display mode for multivariate (`tf_mv`) aesthetics, mirroring
#'   [tf::plot.tf_mv()]: `"trajectory"` draws the planar curve x(t) vs y(t)
#'   (requires exactly 2 components), `"facet"` draws value-vs-arg with one group
#'   per curve and component (add [ggplot2::facet_wrap()] on `.component`). If
#'   `NULL` (default), resolves to `"trajectory"` for 2-component objects and
#'   `"facet"` otherwise. Ignored for univariate tf aesthetics.
#' @param ... Other arguments passed to ggplot2 functions.
#'
#' @details
#' `tf_ggplot` supports the following tf-specific aesthetics:
#' - `tf`: Maps a `tf` object to `y` aesthetic (shorthand for `tf_y`)
#' - `tf_x`: Maps a `tf` object to `x` aesthetic
#' - `tf_y`: Maps a `tf` object to `y` aesthetic
#' - `tf_ymin`: Maps a tf object to ymin aesthetic (for ribbons)
#' - `tf_ymax`: Maps a tf object to ymax aesthetic (for ribbons)
#'
#' When tf aesthetics are used, the data is automatically transformed:
#' - tf objects are evaluated on a common grid
#' - Each function becomes multiple rows (one per evaluation point)
#' - Group identifiers are created to maintain function identity
#' - Non-tf columns are replicated appropriately
#'
#' @return A tf_ggplot object that inherits from ggplot
#'
#' @examples
#' # Basic usage
#' data <- data.frame(
#' id = 1:10,
#' group = sample(c("A", "B"), 10, replace = TRUE)
#' )
#' data$f <- tf_rgp(10)
#'
#' # Method 1: tf aesthetic in constructor
#' tf_ggplot(data, ggplot2::aes(tf = f, color = group)) + ggplot2::geom_line()
#'
#' # Method 2: tf aesthetic in geom (equivalent)
#' tf_ggplot(data) + ggplot2::geom_line(ggplot2::aes(tf = f, color = group))
#'
#' # Confidence bands
#' tf_ggplot(data) +
#'   ggplot2::geom_ribbon(
#'     ggplot2::aes(tf_ymin = mean(f) - sd(f), tf_ymax = mean(f) + sd(f)),
#'     alpha = 0.3
#'   ) +
#'   ggplot2::geom_line(ggplot2::aes(tf = mean(f)))
#'
#' @export
tf_ggplot <- function(
  data = NULL,
  mapping = aes(),
  ...,
  arg = NULL,
  interpolate = TRUE,
  type = NULL
) {
  # Validate inputs
  if (!is.null(data) && !is.data.frame(data)) {
    cli::cli_abort("data must be a data.frame, not {.obj_type_friendly {data}}")
  }

  if (!is_mapping(mapping)) {
    cli::cli_abort(
      "mapping must be created with aes(), not {.obj_type_friendly {mapping}}"
    )
  }

  if (!is.null(arg)) {
    if (!is.numeric(arg) || anyNA(arg)) {
      cli::cli_abort(
        "{.arg arg} must be a numeric vector or a single integer grid length"
      )
    }
    if (length(arg) == 1L && arg < 2) {
      cli::cli_abort("{.arg arg} as grid length must be >= 2, not {arg}")
    }
  }

  if (!is.null(type)) {
    type <- match.arg(type, c("trajectory", "facet"))
  }

  # Create base ggplot object
  p <- ggplot(data = data, mapping = mapping, ...)

  # Convert to tf_ggplot class and add tf-specific attributes
  class(p) <- c("tf_ggplot", class(p))
  attr(p, "tf_arg") <- arg
  attr(p, "tf_interpolate") <- interpolate
  attr(p, "tf_mv_type") <- type
  attr(p, "tf_original_data") <- data
  attr(p, "tf_original_mapping") <- mapping
  attr(p, "all_layers") <- list() # Store all layers in order with metadata
  attr(p, "tf_expression_counter") <- 0 # Counter for unique expression names

  return(p)
}

#' Check if object is a tf_ggplot
#' @param x Object to test
#' @returns `TRUE` if `x` inherits from `"tf_ggplot"`, `FALSE` otherwise.
#' @examples
#' p <- tf_ggplot(data.frame(x = 1))
#' is_tf_ggplot(p)
#' is_tf_ggplot(ggplot2::ggplot())
#' @export
is_tf_ggplot <- function(x) {
  inherits(x, "tf_ggplot")
}

#' Parse aesthetic mappings to separate tf and regular aesthetics
#'
#' @param mapping An aesthetic mapping created with [ggplot2::aes()].
#' @param data Data frame to evaluate expressions against.
#' @returns A list with components `tf_aes`, `scalar_tf_aes`, and `regular_aes`.
#' @examples
#' parse_tf_aesthetics(ggplot2::aes(tf = f, color = group))
#' parse_tf_aesthetics(ggplot2::aes(x = x, y = y))
#' @export
parse_tf_aesthetics <- function(mapping, data = NULL) {
  if (length(mapping) == 0) {
    return(list(tf_aes = list(), scalar_tf_aes = list(), regular_aes = aes()))
  }

  # Get aesthetic names
  aes_names <- names(mapping)

  # Identify tf-specific aesthetics (prefixed with tf_)
  tf_pattern <- "^tf(_.*)?$"
  is_direct_tf_aes <- grepl(tf_pattern, aes_names)

  # For non-tf-prefixed aesthetics, check if they contain tf functions
  is_scalar_tf_aes <- rep(FALSE, length(mapping))
  if (!is.null(data)) {
    for (i in seq_along(mapping)) {
      if (!is_direct_tf_aes[i]) {
        # Only try to extract expression if it's a quosure
        if (rlang::is_quosure(mapping[[i]])) {
          expr <- rlang::quo_get_expr(mapping[[i]])
          # Check if expression contains tf function calls
          expr_text <- rlang::expr_deparse(expr)
          if (grepl("tf_", expr_text)) {
            is_scalar_tf_aes[i] <- TRUE
          }
        }
        # Non-quosure aesthetics (constants) cannot contain tf functions
      }
    }
  }

  # Split aesthetics into three categories
  tf_aes <- mapping[is_direct_tf_aes]
  scalar_tf_aes <- mapping[is_scalar_tf_aes]
  regular_aes <- mapping[!is_direct_tf_aes & !is_scalar_tf_aes]

  # Convert regular_aes back to aes object if not empty
  if (length(regular_aes) > 0) {
    class(regular_aes) <- "uneval"
  } else {
    regular_aes <- aes()
  }

  return(list(
    tf_aes = tf_aes,
    scalar_tf_aes = scalar_tf_aes,
    regular_aes = regular_aes
  ))
}

#' Add layers to tf_ggplot objects
#'
#' @param e1 A tf_ggplot object
#' @param e2 A ggplot2 layer, scale, theme, etc.
#' @returns A modified `tf_ggplot` object.
#' @export
`+.tf_ggplot` <- function(e1, e2) {
  # If e2 is a layer, check if it has tf aesthetics
  if (inherits(e2, "LayerInstance") || inherits(e2, "Layer")) {
    # Reject geom_capellini: fundamentally incompatible (glyph-plot semantics)
    if (inherits(e2$stat, "StatCapellini")) {
      cli::cli_abort(c(
        "{.fn geom_capellini} is not compatible with {.fn tf_ggplot}",
        "i" = "{.fn geom_capellini} uses {.code x}/{.code y} for position and {.code tf} for sparkline shape",
        "i" = "Use {.fn geom_capellini} with plain {.fn ggplot} instead"
      ))
    }

    # Translate old-style tf geoms (geom_spaghetti, geom_meatballs, geom_errorband)
    if (inherits(e2$stat, "StatTf") || inherits(e2$stat, "StatErrorband")) {
      result <- translate_old_tf_layer(e2, e1)
      e1 <- result$plot_obj
      for (new_layer in result$layers) {
        e1 <- e1 + new_layer
      }
      return(e1)
    }

    # Promote tf objects passed as constant params (e.g. geom_line(tf = tf_rgp(5)))
    # into proper layer data + mapping so the normal tf-layer path can handle them.
    .promoted_data <- NULL
    .tf_aes_names <- c("tf", "tf_x", "tf_y", "tf_ymin", "tf_ymax")
    .tf_in_params <- intersect(names(e2$aes_params), .tf_aes_names)
    if (length(.tf_in_params) > 0) {
      .tf_col_list <- list()
      .new_mapping <- e2$mapping %||% aes()
      for (.p in .tf_in_params) {
        .val <- e2$aes_params[[.p]]
        if (is_tf(.val)) {
          .col <- paste0(".", .p, "_const_")
          .tf_col_list[[.col]] <- .val
          .new_mapping[[.p]] <- rlang::sym(.col)
          e2$aes_params[[.p]] <- NULL
        }
      }
      if (length(.tf_col_list) > 0) {
        .promoted_data <- structure(
          .tf_col_list,
          class = "data.frame",
          row.names = seq_len(length(.tf_col_list[[1]]))
        )
        # Do NOT set layer$data here (ggproto reference semantics would mutate
        # the layer permanently, breaking subsequent finalize calls on the same
        # tf_ggplot object). Store promoted data in layer_info instead.
        e2$mapping <- .new_mapping
      }
    }

    # Extract layer mapping
    layer_mapping <- e2$mapping %||% aes()

    # Check if this layer or the plot has tf aesthetics
    parsed_layer_aes <- parse_tf_aesthetics(
      layer_mapping,
      .promoted_data %||% e1$data
    )
    parsed_plot_aes <- parse_tf_aesthetics(e1$mapping, e1$data)

    layer_has_tf <- length(parsed_layer_aes$tf_aes) > 0 ||
      length(parsed_layer_aes$scalar_tf_aes) > 0
    plot_has_tf <- length(parsed_plot_aes$tf_aes) > 0 ||
      length(parsed_plot_aes$scalar_tf_aes) > 0

    if (layer_has_tf || plot_has_tf) {
      # Store this layer with tf aesthetics
      all_layers <- attr(e1, "all_layers")
      if (is.null(all_layers)) all_layers <- list()
      all_layers[[length(all_layers) + 1]] <- list(
        layer = e2,
        layer_mapping = layer_mapping,
        parsed_aes = parsed_layer_aes,
        is_tf_layer = TRUE,
        # Promoted data from tf-param expansion lives here (not in layer$data)
        # so that repeated finalize calls don't see mutated layer state.
        promoted_data = .promoted_data,
        # Snapshot layer-level data at add-time (before any finalize mutation).
        # Used when a layer carries its own data (e.g. from autolayer.tf).
        layer_data_snapshot = if (!inherits(e2$data, "waiver")) e2$data else
          NULL
      )
      attr(e1, "all_layers") <- all_layers

      # Return the tf_ggplot object to continue accumulating layers
      return(e1)
    }
  }

  # Handle regular layers (non-tf layers)
  if (inherits(e2, "LayerInstance") || inherits(e2, "Layer")) {
    layer_mapping <- e2$mapping %||% aes()
    parsed_layer_aes <- parse_tf_aesthetics(layer_mapping, e1$data)
    layer_has_tf <- length(parsed_layer_aes$tf_aes) > 0 ||
      length(parsed_layer_aes$scalar_tf_aes) > 0

    if (!layer_has_tf) {
      # This is a regular (non-tf) layer - store it instead of finalizing immediately

      # Use the plot's data only when the layer brought none of its own (a layer
      # carrying its own data, e.g. from autolayer(), must keep it).
      if (inherits(e2$data, "waiver") || is.null(e2$data)) {
        e2$data <- e1$data
      }

      # Check for potential scale conflicts
      all_layers <- attr(e1, "all_layers")
      tf_layers_exist <- (!is.null(all_layers) &&
        any(sapply(all_layers, function(x) x$is_tf_layer))) ||
        length(parse_tf_aesthetics(e1$mapping, e1$data)$tf_aes) > 0

      if (tf_layers_exist) {
        # Check if this regular layer uses aesthetics that might conflict with tf layers
        regular_aes_names <- names(layer_mapping)
        tf_conflicting_aes <- c("x", "y", "ymin", "ymax") # aesthetics that tf layers commonly use

        potential_conflicts <- intersect(regular_aes_names, tf_conflicting_aes)
        if (length(potential_conflicts) > 0) {
          cli::cli_warn(c(
            "Potential scale conflict detected: regular layer uses {.field {potential_conflicts}} aesthetic(s) while tf layers also modify these scales",
            "i" = "This may result in unexpected scale ranges or behavior",
            "i" = "Consider using different aesthetics or ensuring data ranges are compatible"
          ))
        }
      }

      # Store the regular layer in order
      all_layers <- attr(e1, "all_layers")
      if (is.null(all_layers)) all_layers <- list()
      all_layers[[length(all_layers) + 1]] <- list(
        layer = e2,
        layer_mapping = layer_mapping,
        parsed_aes = parsed_layer_aes,
        is_tf_layer = FALSE
      )
      attr(e1, "all_layers") <- all_layers

      # Return the tf_ggplot object to continue accumulating layers
      return(e1)
    }
  }

  # Non-layer object (like themes, scales, etc.) - need to finalize tf_ggplot
  all_layers <- attr(e1, "all_layers")
  if (
    (!is.null(all_layers) && length(all_layers) > 0) ||
      length(parse_tf_aesthetics(e1$mapping, e1$data)$tf_aes) > 0 ||
      length(parse_tf_aesthetics(e1$mapping, e1$data)$scalar_tf_aes) > 0
  ) {
    # Convert tf_ggplot with all accumulated layers
    regular_plot <- finalize_tf_ggplot(e1)
    return(regular_plot + e2)
  }

  # No tf aesthetics at all - convert to regular ggplot
  regular_plot <- ggplot(data = e1$data, mapping = e1$mapping)
  regular_plot$theme <- e1$theme
  regular_plot$coordinates <- e1$coordinates
  regular_plot$facet <- e1$facet
  regular_plot$labels <- e1$labels

  return(regular_plot + e2)
}

#' Finalize tf_ggplot by processing all tf layers independently
#'
#' Each layer is evaluated on its own natural argument grid (Option B architecture).
#' The base ggplot has `data = NULL`; each layer receives its own long-format data.
#'
#' @param tf_plot A tf_ggplot object with accumulated layers
#' @return Regular ggplot object with all layers properly transformed
#' @keywords internal
finalize_tf_ggplot <- function(tf_plot) {
  user_arg <- attr(tf_plot, "tf_arg")
  interpolate <- attr(tf_plot, "tf_interpolate") %||% TRUE
  mv_type <- attr(tf_plot, "tf_mv_type")
  original_data <- tf_plot$data
  all_layers <- attr(tf_plot, "all_layers") %||% list()

  # Parse plot-level aesthetics
  parsed_plot_aes <- parse_tf_aesthetics(tf_plot$mapping, original_data)

  # Pre-evaluate all scalar tf aesthetics (e.g., color = tf_depth(f)) into enriched_data.
  # These produce one scalar per original row and are replicated automatically via join.
  enriched_data <- original_data
  scalar_col_map <- list() # key -> col_name in enriched_data

  n_data_rows <- nrow(enriched_data)
  eval_scalar_tf_aes <- function(aes_name, quo) {
    result <- tryCatch(
      rlang::eval_tidy(quo, data = enriched_data),
      error = function(e)
        cli::cli_abort(
          "Error evaluating scalar tf aesthetic {.code {aes_name}}: {e$message}"
        )
    )
    if (!length(result) %in% c(1L, n_data_rows)) {
      cli::cli_abort(c(
        "Scalar tf aesthetic {.code {aes_name}} returned {length(result)} value(s)",
        "i" = "Expected 1 or {n_data_rows} (one per row of plot data)"
      ))
    }
    result
  }

  for (aes_name in names(parsed_plot_aes$scalar_tf_aes)) {
    quo <- parsed_plot_aes$scalar_tf_aes[[aes_name]]
    result <- eval_scalar_tf_aes(aes_name, quo)
    col_nm <- make_safe_column_name(
      paste0(".s.", aes_name),
      names(enriched_data)
    )
    enriched_data[[col_nm]] <- result
    scalar_col_map[[aes_name]] <- col_nm
  }
  for (i in seq_along(all_layers)) {
    if (!all_layers[[i]]$is_tf_layer) next
    for (aes_name in names(all_layers[[i]]$parsed_aes$scalar_tf_aes)) {
      key <- paste0(aes_name, ".layer.", i)
      quo <- all_layers[[i]]$parsed_aes$scalar_tf_aes[[aes_name]]
      result <- eval_scalar_tf_aes(aes_name, quo)
      col_nm <- make_safe_column_name(paste0(".s.", key), names(enriched_data))
      enriched_data[[col_nm]] <- result
      scalar_col_map[[key]] <- col_nm
    }
  }

  # Base mapping: regular plot-level aes + scalar tf aes remapped to column names.
  # Does NOT include x/y/group from tf aes -- those are per-layer.
  base_mapping <- parsed_plot_aes$regular_aes
  for (aes_name in names(parsed_plot_aes$scalar_tf_aes)) {
    base_mapping[[aes_name]] <- rlang::sym(scalar_col_map[[aes_name]])
  }

  # Create base plot with data = NULL; each layer brings its own data
  regular_plot <- ggplot(data = NULL, mapping = base_mapping)
  raw_tf_axis_labels <- NULL

  for (i in seq_along(all_layers)) {
    layer_info <- all_layers[[i]]
    layer <- layer_info$layer

    if (layer_info$is_tf_layer) {
      # Use promoted data (from tf-param expansion) when present;
      # fall back to plot-level enriched_data.  Never use layer$data here --
      # ggproto reference semantics mean layer$data gets mutated by a previous
      # finalize call, breaking repeated ggplot_build() calls on the same object.
      effective_data <- layer_info$promoted_data %||%
        layer_info$layer_data_snapshot %||%
        enriched_data
      result <- if (inherits(layer$stat, "StatFboxplot")) {
        build_raw_tf_layer_data(
          layer_info = layer_info,
          plot_mapping = tf_plot$mapping,
          layer_idx = i,
          enriched_data = effective_data
        )
      } else {
        build_tf_layer_data(
          layer_info = layer_info,
          plot_tf_aes = parsed_plot_aes$tf_aes,
          scalar_col_map = scalar_col_map,
          layer_idx = i,
          enriched_data = effective_data,
          user_arg = user_arg,
          interpolate = interpolate,
          mv_type = mv_type
        )
      }
      if (!is.null(result)) {
        layer$data <- result$long_data
        layer$mapping <- result$new_mapping
        if (!is.null(result$stat_params)) {
          layer$stat_params <- utils::modifyList(
            layer$stat_params,
            result$stat_params
          )
        }
        if (is.null(raw_tf_axis_labels) && !is.null(result$axis_labels)) {
          raw_tf_axis_labels <- result$axis_labels
        }
      } else if (is.null(layer$data)) {
        layer$data <- enriched_data
      }
    } else if (is.null(layer$data)) {
      layer$data <- enriched_data
    }

    regular_plot <- regular_plot + layer
  }

  regular_plot$theme <- tf_plot$theme
  regular_plot$coordinates <- tf_plot$coordinates
  regular_plot$facet <- tf_plot$facet
  regular_plot$labels <- tf_plot$labels

  # Fix legend titles for scalar tf aesthetics (e.g. linewidth = tf_fmean(f)):
  # the internal column name (.s.linewidth) leaks into the legend; replace with
  # the original expression text.
  for (aes_name in names(parsed_plot_aes$scalar_tf_aes)) {
    if (is.null(regular_plot$labels[[aes_name]])) {
      quo <- parsed_plot_aes$scalar_tf_aes[[aes_name]]
      expr_text <- paste(
        rlang::expr_deparse(rlang::quo_get_expr(quo)),
        collapse = ""
      )
      regular_plot$labels[[aes_name]] <- expr_text
    }
  }

  if (!is.null(raw_tf_axis_labels)) {
    if (is.null(regular_plot$labels$x)) {
      regular_plot$labels$x <- raw_tf_axis_labels$x
    }
    if (is.null(regular_plot$labels$y)) {
      regular_plot$labels$y <- raw_tf_axis_labels$y
    }
  }

  regular_plot
}

combine_layer_mappings <- function(
  plot_mapping,
  layer_mapping,
  inherit.aes = TRUE
) {
  plot_mapping <- plot_mapping %||% aes()
  layer_mapping <- layer_mapping %||% aes()

  combined <- if (isTRUE(inherit.aes)) {
    c(
      plot_mapping[!names(plot_mapping) %in% names(layer_mapping)],
      layer_mapping
    )
  } else {
    layer_mapping
  }
  class(combined) <- "uneval"
  combined
}

build_raw_tf_layer_data <- function(
  layer_info,
  plot_mapping,
  layer_idx,
  enriched_data
) {
  layer <- layer_info$layer
  combined_mapping <- combine_layer_mappings(
    plot_mapping = plot_mapping,
    layer_mapping = layer$mapping,
    inherit.aes = layer$inherit.aes
  )

  if (inherits(layer$stat, "StatFboxplot")) {
    normalized <- normalize_fboxplot_mapping(combined_mapping)
    combined_mapping <- normalized$mapping
    stat_params <- list(use_group_aes = normalized$use_group_aes)
    axis_labels <- infer_tf_axis_labels(
      combined_mapping,
      orientation = layer$stat_params$orientation %||% NA
    )
  } else {
    stat_params <- NULL
    axis_labels <- NULL
  }

  list(
    long_data = enriched_data,
    new_mapping = combined_mapping,
    stat_params = stat_params,
    axis_labels = axis_labels
  )
}

infer_tf_axis_labels <- function(mapping, orientation = NA) {
  tf_name <- intersect(c("tf", "tf_y"), names(mapping))[1]
  if (is.na(tf_name) || is.null(tf_name)) {
    return(NULL)
  }

  tf_expr <- mapping[[tf_name]]
  if (!rlang::is_quosure(tf_expr)) {
    return(NULL)
  }

  expr_text <- paste(
    rlang::expr_deparse(rlang::quo_get_expr(tf_expr)),
    collapse = ""
  )

  orientation <- if (is.null(orientation) || is.na(orientation)) {
    "x"
  } else {
    match.arg(orientation, c("x", "y"))
  }

  if (orientation == "y") {
    list(
      x = expr_text,
      y = paste0(expr_text, ".arg")
    )
  } else {
    list(
      x = paste0(expr_text, ".arg"),
      y = expr_text
    )
  }
}

# Resolve a user-supplied evaluation grid: a single integer means "this many
# equidistant points over the domain of `f`"; numeric vectors and NULL pass
# through unchanged.
resolve_arg_grid <- function(arg, f) {
  if (!is.null(arg) && length(arg) == 1L) {
    domain <- tf_domain(f)
    arg <- seq(domain[1], domain[2], length.out = as.integer(arg))
  }
  arg
}

# Align enriched_data to one row per function: synthesise an empty frame when
# there is no plot-level data (e.g. tf_ggplot() + geom_line(aes(tf = tf_rgp(5)))),
# keep the first row for length-1 aesthetics, recycle a single row, abort
# otherwise. `what` names the tf aesthetic(s) in the abort message.
align_layer_data_rows <- function(enriched_data, n_funcs, what) {
  .n_enriched <- nrow(enriched_data) # NULL for NULL / waiver / non-data-frames
  if (is.null(.n_enriched) || .n_enriched == 0) {
    enriched_data <- structure(
      list(),
      class = "data.frame",
      row.names = seq_len(n_funcs)
    )
  }
  n_rows <- nrow(enriched_data)
  if (n_rows != n_funcs) {
    if (n_funcs == 1L && n_rows > 0) {
      enriched_data <- enriched_data[1, , drop = FALSE]
    } else if (n_rows == 1L && n_funcs > 1L) {
      enriched_data <- enriched_data[rep(1L, n_funcs), , drop = FALSE]
    } else {
      cli::cli_abort(c(
        paste0("Layer data cannot be aligned with ", what, "."),
        "i" = "Data has {n_rows} row(s) but there are {n_funcs} function(s)."
      ))
    }
  }
  enriched_data
}

# Point layer-level scalar tf aes (e.g. colour = tf_depth(f)) at their
# pre-evaluated columns (already joined in from enriched_data).
remap_scalar_tf_aes <- function(
  mapping,
  scalar_tf_aes,
  scalar_col_map,
  layer_idx
) {
  for (aes_name in names(scalar_tf_aes)) {
    key <- paste0(aes_name, ".layer.", layer_idx)
    if (key %in% names(scalar_col_map)) {
      mapping[[aes_name]] <- rlang::sym(scalar_col_map[[key]])
    }
  }
  mapping
}

#' Build long-format data and new mapping for a single tf layer
#'
#' Evaluates the layer's tf aesthetics independently on their own natural grid
#' (or the user-specified grid). Called by [finalize_tf_ggplot()] per layer.
#'
#' @param layer_info List from `all_layers` with `layer`, `parsed_aes`, `is_tf_layer`
#' @param plot_tf_aes tf aesthetics from the plot level (used when `inherit.aes = TRUE`)
#' @param scalar_col_map Named list: key -> column name in enriched_data
#' @param layer_idx Integer index of this layer (for keying layer-level scalar aes)
#' @param enriched_data Data frame with original data + pre-evaluated scalar tf columns
#' @param user_arg Optional evaluation grid (overrides natural grid)
#' @param interpolate Whether to interpolate tf objects to `arg`
#' @return `NULL` when no effective tf aes, otherwise `list(long_data, new_mapping)`
#' @keywords internal
build_tf_layer_data <- function(
  layer_info,
  plot_tf_aes,
  scalar_col_map,
  layer_idx,
  enriched_data,
  user_arg,
  interpolate,
  mv_type = NULL
) {
  layer <- layer_info$layer
  parsed_aes <- layer_info$parsed_aes

  # Effective tf aes: plot-level + layer-level, with layer overriding (respects inherit.aes)
  effective_tf_aes <- if (isTRUE(layer$inherit.aes)) {
    c(
      plot_tf_aes[!names(plot_tf_aes) %in% names(parsed_aes$tf_aes)],
      parsed_aes$tf_aes
    )
  } else {
    parsed_aes$tf_aes
  }

  if (length(effective_tf_aes) == 0) {
    return(NULL)
  }

  # Planar curve x(t) vs y(t): both tf_x and tf_y present in this layer. Then
  # tf_y must not overwrite x with its arg grid (x comes from tf_x's values).
  planar_xy <- all(c("tf_x", "tf_y") %in% names(effective_tf_aes))

  # Evaluate all tf expressions
  tf_objects <- vector("list", length(effective_tf_aes))
  names(tf_objects) <- names(effective_tf_aes)
  for (aes_name in names(effective_tf_aes)) {
    quo <- effective_tf_aes[[aes_name]]
    tf_obj <- tryCatch(
      rlang::eval_tidy(quo, data = enriched_data),
      error = function(e) {
        expr_text <- paste(
          rlang::expr_deparse(rlang::quo_get_expr(quo)),
          collapse = ""
        )
        cli::cli_abort(
          "Error evaluating tf aesthetic {.code {expr_text}}: {e$message}"
        )
      }
    )
    if (!is_tf(tf_obj)) {
      expr_text <- paste(
        rlang::expr_deparse(rlang::quo_get_expr(quo)),
        collapse = ""
      )
      cli::cli_abort(
        "tf aesthetic {.code {aes_name} = {expr_text}} must evaluate to a tf object, not {.obj_type_friendly {tf_obj}}"
      )
    }
    tf_objects[[aes_name]] <- tf_obj
  }

  # Multivariate (tf_mv) aesthetic: handled wholesale by a dedicated builder so
  # the object never enters the univariate primary/secondary unnest path below.
  is_mv <- vapply(tf_objects, is_tf_mv, logical(1))
  if (any(is_mv)) {
    if (length(effective_tf_aes) > 1L) {
      cli::cli_abort(c(
        "A {.cls tf_mv} aesthetic cannot be combined with other tf aesthetics in one layer.",
        "i" = "Map the {.cls tf_mv} object on its own with {.code aes(tf = ...)}."
      ))
    }
    aes_name <- names(effective_tf_aes)[1]
    if (!aes_name %in% c("tf", "tf_y")) {
      cli::cli_abort(c(
        "A {.cls tf_mv} object must be mapped with {.code aes(tf = ...)}.",
        "x" = "Got {.code aes({aes_name} = ...)}."
      ))
    }
    return(build_tf_mv_layer_data(
      mv = tf_objects[[1]],
      mv_quo = effective_tf_aes[[1]],
      geom = layer$geom,
      parsed_aes = parsed_aes,
      scalar_col_map = scalar_col_map,
      layer_idx = layer_idx,
      enriched_data = enriched_data,
      user_arg = user_arg,
      interpolate = interpolate,
      mv_type = mv_type
    ))
  }

  tf_lengths <- vapply(tf_objects, length, integer(1))
  common_n_funcs <- max(tf_lengths)
  if (!all(tf_lengths %in% c(1L, common_n_funcs))) {
    cli::cli_abort(c(
      "All tf aesthetics in a layer must have matching lengths or length 1",
      "i" = "Observed lengths: {paste(tf_lengths, collapse = ', ')}"
    ))
  }

  primary_idx <- which(tf_lengths == common_n_funcs)[1]
  if (!identical(primary_idx, 1L)) {
    tf_objects <- tf_objects[c(
      primary_idx,
      setdiff(seq_along(tf_objects), primary_idx)
    )]
    effective_tf_aes <- effective_tf_aes[c(
      primary_idx,
      setdiff(seq_along(effective_tf_aes), primary_idx)
    )]
    tf_lengths <- tf_lengths[c(
      primary_idx,
      setdiff(seq_along(tf_lengths), primary_idx)
    )]
  }

  # This layer's evaluation grid: user-specified or natural grid of first tf object.
  arg <- resolve_arg_grid(user_arg, tf_objects[[1]])
  if (is.null(arg)) {
    first_arg <- tf_arg(tf_objects[[1]])
    arg <- if (is.list(first_arg)) first_arg[[1]] else first_arg
  }

  n_funcs <- common_n_funcs
  enriched_data <- align_layer_data_rows(
    enriched_data,
    n_funcs,
    "the evaluated tf aesthetics"
  )
  n_rows <- nrow(enriched_data)
  n_grid <- length(arg)
  if (n_rows > 200 && n_grid > 100) {
    cli::cli_warn(
      c(
        "Large data expansion: {n_rows} functions \u00d7 {n_grid} grid points
       = {n_rows * n_grid} rows",
        "i" = "Use {.arg arg} in {.fn tf_ggplot} to specify a coarser
       evaluation grid (integer for grid length, numeric vector for
       specific points)"
      ),
      .frequency = "regularly",
      .frequency_id = "tidyfun_large_expansion"
    )
  }

  # Unnest first tf aesthetic to long format
  first_aes_name <- names(tf_objects)[1]
  first_tf <- tf_objects[[1]]

  # Use expression text for column names (produces meaningful axis labels).
  # For simple column refs like aes(tf = curves), val_col = "curves".
  # For complex expressions like aes(tf = func1 + func2), val_col = "func1 + func2".
  first_quo <- effective_tf_aes[[first_aes_name]]
  first_expr <- rlang::quo_get_expr(first_quo)
  expr_text <- paste(rlang::expr_deparse(first_expr), collapse = "")
  # Exclude the source column name from conflict check (it will be removed before join)
  excl <- if (is.symbol(first_expr)) as.character(first_expr) else character(0)
  safe_name <- make_safe_column_name(
    expr_text,
    existing_names = setdiff(names(enriched_data), excl)
  )
  val_col <- safe_name
  arg_col <- paste0(safe_name, ".arg")
  id_col <- paste0(safe_name, ".id")

  tf_long <- suppressMessages(tf_unnest(
    first_tf,
    arg = arg,
    interpolate = interpolate
  ))
  # tf_long has columns: id (ordered factor), arg, value
  # Rename immediately to avoid conflicts with user data columns of the same name
  names(tf_long)[names(tf_long) == "id"] <- id_col
  names(tf_long)[names(tf_long) == "arg"] <- arg_col
  names(tf_long)[names(tf_long) == "value"] <- val_col

  tf_long$.row_id_ <- rep(seq_len(n_rows), each = n_grid)
  tf_long <- tf_long[!is.na(tf_long[[val_col]]), ]

  # Join with enriched_data to replicate covariates + scalar columns
  work_data <- enriched_data
  work_data$.row_id_ <- seq_len(n_rows)
  # Remove original tf columns to avoid conflicts during join
  for (aes_name in names(effective_tf_aes)) {
    expr <- rlang::quo_get_expr(effective_tf_aes[[aes_name]])
    if (is.symbol(expr)) {
      col_nm <- as.character(expr)
      if (col_nm %in% names(work_data)) work_data[[col_nm]] <- NULL
    }
  }

  long_data <- left_join(tf_long, work_data, by = ".row_id_") |>
    select(-.row_id_)

  # Build the layer mapping
  new_mapping <- parsed_aes$regular_aes

  for (i in seq_along(tf_objects)) {
    aes_name_i <- names(tf_objects)[i]
    if (i == 1) {
      v_col <- val_col
      a_col <- arg_col
      g_col <- id_col
    } else {
      # Evaluate additional tf aes on the FULL arg grid (not NA-filtered subset).
      # Using curr_arg (from long_data after NA filtering) would silently drop points
      # if the primary tf had NAs, misaligning the secondary aesthetic.
      tf_vals <- tf_evaluate(tf_objects[[i]], arg = arg)
      # Use expression text for meaningful column names
      quo_i <- effective_tf_aes[[aes_name_i]]
      expr_i <- rlang::quo_get_expr(quo_i)
      expr_text_i <- paste(rlang::expr_deparse(expr_i), collapse = "")
      excl_i <- if (is.symbol(expr_i)) as.character(expr_i) else character(0)
      safe_i <- make_safe_column_name(
        expr_text_i,
        existing_names = setdiff(
          c(names(long_data), names(enriched_data)),
          excl_i
        )
      )
      v_col <- safe_i
      a_col <- paste0(safe_i, ".arg")
      g_col <- paste0(safe_i, ".id")
      n_tf_i <- length(tf_vals)
      if (n_tf_i == 1L && n_funcs > 1L) {
        long_data[[v_col]] <- unlist(tf_vals)[
          match(long_data[[arg_col]], arg)
        ]
        long_data[[g_col]] <- long_data[[id_col]]
      } else {
        # Build full secondary long-form, then left-join on (id, arg) to align with primary
        sec_long <- data.frame(
          .row_id_i_ = rep(seq_len(n_tf_i), each = length(arg)),
          .arg_i_ = rep(arg, n_tf_i),
          .val_i_ = unlist(tf_vals)
        )
        # Attach secondary values to long_data by matching row-id and arg position
        long_data[[v_col]] <- sec_long$.val_i_[
          match(
            paste(as.integer(long_data[[id_col]]), long_data[[arg_col]]),
            paste(sec_long$.row_id_i_, sec_long$.arg_i_)
          )
        ]
        long_data[[g_col]] <- long_data[[id_col]]
      }
      long_data[[a_col]] <- long_data[[arg_col]]
    }
    new_mapping <- add_tf_aes_to_mapping(
      new_mapping,
      aes_name_i,
      v_col,
      a_col,
      g_col,
      planar_xy = planar_xy
    )
  }

  new_mapping <- remap_scalar_tf_aes(
    new_mapping,
    parsed_aes$scalar_tf_aes,
    scalar_col_map,
    layer_idx
  )

  list(long_data = long_data, new_mapping = new_mapping)
}

# Resolve the tf_mv display type: NULL defaults to "trajectory" for exactly 2
# components and "facet" otherwise; "trajectory" requires exactly 2 components.
resolve_tf_mv_type <- function(type, d, call = rlang::caller_env()) {
  type <- match.arg(
    type %||% if (d == 2L) "trajectory" else "facet",
    c("trajectory", "facet")
  )
  if (type == "trajectory" && d != 2L) {
    cli::cli_abort(
      c(
        "{.code type = \"trajectory\"} requires a {.cls tf_mv} with exactly 2 components.",
        "x" = "This object has {d} component{?s}."
      ),
      call = call
    )
  }
  type
}

#' Build long-format data and mapping for a single multivariate (tf_mv) aesthetic
#'
#' Two displays, mirroring [tf::plot.tf_mv()]: `"trajectory"` (the planar curve
#' x(t) vs y(t), default for `d == 2`) and `"facet"` (value-vs-arg, one group per
#' curve x component, default otherwise). The caller (a `tf_mv` aesthetic mapped
#' via `aes(tf = ...)`) guarantees this is the only tf aesthetic in the layer.
#'
#' @param mv The evaluated `tf_mv` object.
#' @param mv_quo The quosure for the aesthetic (used for axis labels / source column).
#' @param geom The layer geom, used to reject geoms that reorder trajectories.
#' @param mv_type `"trajectory"`, `"facet"`, or `NULL` (resolve from `d`).
#' @inheritParams build_tf_layer_data
#' @returns `list(long_data, new_mapping, axis_labels)`.
#' @keywords internal
build_tf_mv_layer_data <- function(
  mv,
  mv_quo,
  geom,
  parsed_aes,
  scalar_col_map,
  layer_idx,
  enriched_data,
  user_arg,
  interpolate,
  mv_type = NULL
) {
  d <- tf_ncomp(mv)
  comp_names <- names(tf_components(mv))
  type <- resolve_tf_mv_type(mv_type, d)
  if (type == "trajectory" && inherits(geom, "GeomLine")) {
    cli::cli_abort(c(
      "{.fn geom_line} cannot draw {.cls tf_mv} trajectories correctly.",
      "i" = "Use {.fn geom_path} to preserve argument order."
    ))
  }

  # NULL `arg` means the natural/union grid, resolved downstream
  arg <- resolve_arg_grid(user_arg, mv)

  n_funcs <- length(mv)
  enriched_data <- align_layer_data_rows(
    enriched_data,
    n_funcs,
    "the {.cls tf_mv} aesthetic"
  )
  n_rows <- nrow(enriched_data)

  work_data <- enriched_data
  mv_expr <- rlang::quo_get_expr(mv_quo)
  if (rlang::is_symbol(mv_expr)) {
    src <- rlang::as_string(mv_expr)
    if (src %in% names(work_data)) work_data[[src]] <- NULL
  }
  # protect the generated columns against same-named user covariates (which
  # would get .x/.y-suffixed by the join while the mapping still references
  # the bare name). `.component` is user-facing (facet_wrap(~.component)) and
  # cannot be renamed, so a colliding covariate is an error.
  if (".component" %in% names(work_data)) {
    cli::cli_abort(c(
      "Column {.val .component} in {.arg data} collides with the component
       column generated for {.cls tf_mv} layers.",
      i = "Rename that column: {.val .component} is reserved for faceting
       mv displays (e.g. {.code facet_wrap(~.component)})."
    ))
  }
  gen_col <- vapply(
    c(
      ".row_id_",
      ".mv_id",
      ".mv_arg",
      ".mv_value",
      ".mv_x",
      ".mv_y",
      ".mv_group"
    ),
    make_safe_column_name,
    character(1),
    existing_names = names(work_data)
  )
  work_data[[gen_col[[".row_id_"]]]] <- seq_len(n_rows)
  mv_label <- paste(rlang::expr_deparse(mv_expr), collapse = "")

  new_mapping <- parsed_aes$regular_aes

  if (type == "trajectory") {
    tj <- .tf_mv_trajectory_long(mv, arg = arg, interpolate = interpolate)
    names(tj) <- unname(gen_col[names(tj)])
    long_data <- left_join(tj, work_data, by = gen_col[[".row_id_"]]) |>
      select(-all_of(gen_col[[".row_id_"]]))
    new_mapping$x <- rlang::sym(gen_col[[".mv_x"]])
    new_mapping$y <- rlang::sym(gen_col[[".mv_y"]])
    new_mapping$group <- rlang::sym(gen_col[[".mv_id"]])
    axis_labels <- list(x = comp_names[1], y = comp_names[2])
  } else {
    lg <- .tf_mv_unnest_long(mv, arg = arg, interpolate = interpolate)
    lg[[gen_col[[".row_id_"]]]] <- as.integer(lg$id)
    # rename BEFORE the join so user covariates named "id"/"arg"/"value" (or
    # any ".mv_*") cannot collide
    names(lg)[names(lg) == "id"] <- gen_col[[".mv_id"]]
    names(lg)[names(lg) == "arg"] <- gen_col[[".mv_arg"]]
    names(lg)[names(lg) == "value"] <- gen_col[[".mv_value"]]
    names(lg)[names(lg) == ".mv_group"] <- gen_col[[".mv_group"]]
    long_data <- left_join(lg, work_data, by = gen_col[[".row_id_"]]) |>
      select(-all_of(gen_col[[".row_id_"]]))
    new_mapping$x <- rlang::sym(gen_col[[".mv_arg"]])
    new_mapping$y <- rlang::sym(gen_col[[".mv_value"]])
    new_mapping$group <- rlang::sym(gen_col[[".mv_group"]])
    axis_labels <- list(x = "arg", y = mv_label)
  }

  new_mapping <- remap_scalar_tf_aes(
    new_mapping,
    parsed_aes$scalar_tf_aes,
    scalar_col_map,
    layer_idx
  )

  list(
    long_data = long_data,
    new_mapping = new_mapping,
    axis_labels = axis_labels
  )
}

# Long form for a 2-component tf_mv planar curve: (.row_id_, .mv_id, .mv_arg,
# .mv_x, .mv_y), one row per (curve, grid point), ordered by (curve, arg).
# Both components are evaluated by tf::as.matrix.tf_mv on the sorted union of
# their argument grids (or `arg` if given), interpolating, with NA outside a
# component's observed range so geom_path() breaks the curve there.
.tf_mv_trajectory_long <- function(mv, arg = NULL, interpolate = TRUE) {
  if (isFALSE(interpolate)) {
    cli::cli_inform(
      c(
        "{.arg interpolate = FALSE} is ignored for {.cls tf_mv} trajectory plots.",
        "i" = "Trajectory plots require paired component values on a common argument grid, so components are evaluated with interpolation."
      ),
      .frequency = "regularly",
      .frequency_id = "tf_mv_trajectory_interpolate"
    )
  }
  # compute the grid here (rather than letting as.matrix resolve it) because
  # it is needed as an exact numeric column below
  grid <- arg %||%
    sort(unique(as.numeric(
      unlist(map(tf_components(mv), tf_arg), use.names = FALSE)
    )))
  if (length(mv) == 0L || length(grid) == 0L) {
    # empty layer for zero-length tf_mv columns / empty grids
    return(data.frame(
      .row_id_ = integer(0),
      .mv_id = ordered(character(0)),
      .mv_arg = numeric(0),
      .mv_x = numeric(0),
      .mv_y = numeric(0)
    ))
  }
  arr <- as.matrix(mv, arg = grid, interpolate = TRUE) # [curve, arg, component]
  n <- dim(arr)[1L]
  g <- length(grid)
  id <- unique_id(names(mv)) %||% seq_len(n)
  data.frame(
    .row_id_ = rep(seq_len(n), each = g),
    .mv_id = ordered(rep(id, each = g), levels = id),
    .mv_arg = rep(grid, n),
    .mv_x = as.vector(t(arr[,, 1L])),
    .mv_y = as.vector(t(arr[,, 2L]))
  )
}

#' Add a tf aesthetic to an ggplot2 mapping object
#'
#' @param planar_xy `TRUE` when both `tf_x` and `tf_y` are present in the same
#'   layer (a planar curve x(t) vs y(t)). In that case `tf_y` must NOT overwrite
#'   `x` with its arg grid -- `x` comes from `tf_x`'s function values.
#' @keywords internal
add_tf_aes_to_mapping <- function(
  mapping,
  aes_name,
  val_col,
  arg_col,
  id_col,
  planar_xy = FALSE
) {
  if (aes_name %in% c("tf", "tf_y")) {
    mapping$y <- rlang::sym(val_col)
    if (!planar_xy) mapping$x <- rlang::sym(arg_col)
    mapping$group <- rlang::sym(id_col)
  } else if (aes_name == "tf_x") {
    mapping$x <- rlang::sym(val_col)
    if (is.null(mapping$group)) mapping$group <- rlang::sym(id_col)
  } else if (aes_name == "tf_ymin") {
    mapping$ymin <- rlang::sym(val_col)
    if (is.null(mapping$x)) mapping$x <- rlang::sym(arg_col)
    if (is.null(mapping$group)) mapping$group <- rlang::sym(id_col)
  } else if (aes_name == "tf_ymax") {
    mapping$ymax <- rlang::sym(val_col)
    if (is.null(mapping$x)) mapping$x <- rlang::sym(arg_col)
    if (is.null(mapping$group)) mapping$group <- rlang::sym(id_col)
  }
  mapping
}

# Translate old-style tf geoms to tf_ggplot-compatible layers ----------------

#' Translate a geom_spaghetti/geom_meatballs/geom_errorband layer for tf_ggplot
#'
#' Remaps old-style y/ymin/ymax tf aesthetics to tf/tf_ymin/tf_ymax and
#' substitutes the appropriate standard ggplot2 geom.
#'
#' @param layer A LayerInstance using StatTf or StatErrorband
#' @param e1 The tf_ggplot object (may be modified to clean up plot-level y)
#' @return A list with `plot_obj` (possibly modified e1) and `layers` (list of
#'   new layers to add)
#' @keywords internal
translate_old_tf_layer <- function(layer, e1) {
  layer_mapping <- layer$mapping %||% aes()
  plot_mapping <- e1$mapping %||% aes()
  extra_params <- layer$aes_params

  make_layer <- function(geom_fn, mapping) {
    layer_data <- if (!inherits(layer$data, "waiver")) layer$data else NULL
    do.call(
      geom_fn,
      c(
        list(
          mapping = mapping,
          data = layer_data,
          position = layer$position,
          show.legend = layer$show.legend,
          inherit.aes = layer$inherit.aes
        ),
        extra_params
      )
    )
  }

  if (inherits(layer$stat, "StatTf")) {
    if ("y" %in% names(layer_mapping)) {
      new_mapping <- layer_mapping
      new_mapping[["tf"]] <- new_mapping[["y"]]
      new_mapping[["y"]] <- NULL
    } else if ("y" %in% names(plot_mapping)) {
      new_mapping <- layer_mapping
      new_mapping[["tf"]] <- plot_mapping[["y"]]
      e1$mapping[["y"]] <- NULL # remove from plot mapping to avoid conflicts
    } else {
      cli::cli_warn(
        "Cannot translate {.fn geom_spaghetti}/{.fn geom_meatballs}: no {.code y} aesthetic found"
      )
      return(list(plot_obj = e1, layers = list(layer)))
    }

    cli::cli_inform(
      c(
        "i" = "{.fn geom_spaghetti} layer automatically translated for {.fn tf_ggplot}",
        "*" = "Use {.fn geom_line} with {.code aes(tf = f)} directly to silence this"
      ),
      .frequency = "regularly",
      .frequency_id = "translate_spaghetti"
    )

    layers <- if (inherits(layer$geom, "GeomMeatball")) {
      list(
        make_layer(geom_line, new_mapping),
        make_layer(geom_point, new_mapping)
      )
    } else {
      list(make_layer(geom_line, new_mapping))
    }
    list(plot_obj = e1, layers = layers)
  } else if (inherits(layer$stat, "StatErrorband")) {
    new_mapping <- layer_mapping
    if ("ymin" %in% names(new_mapping)) {
      new_mapping[["tf_ymin"]] <- new_mapping[["ymin"]]
      new_mapping[["ymin"]] <- NULL
    }
    if ("ymax" %in% names(new_mapping)) {
      new_mapping[["tf_ymax"]] <- new_mapping[["ymax"]]
      new_mapping[["ymax"]] <- NULL
    }

    cli::cli_inform(
      c(
        "i" = "{.fn geom_errorband} layer automatically translated for {.fn tf_ggplot}",
        "*" = "Use {.fn geom_ribbon} with {.code aes(tf_ymin = lo, tf_ymax = hi)} directly to silence this"
      ),
      .frequency = "regularly",
      .frequency_id = "translate_errorband"
    )

    list(plot_obj = e1, layers = list(make_layer(geom_ribbon, new_mapping)))
  }
}

# Helper function to check if mapping is an aes object
is_mapping <- function(x) {
  inherits(x, "uneval")
}

# Helper function to create safe column names from expression text
make_safe_column_name <- function(expr_text, existing_names = character(0)) {
  # For simple names (just letters, numbers, underscore), use as-is if valid
  if (
    grepl("^[a-zA-Z][a-zA-Z0-9_]*$", expr_text) &&
      !expr_text %in% existing_names
  ) {
    return(expr_text)
  }

  # For complex expressions, use the original text as column name
  # R allows any string as a column name if accessed properly
  safe_name <- expr_text

  # If the resulting name conflicts with existing names, add suffix
  original_safe_name <- safe_name
  counter <- 1
  while (safe_name %in% existing_names) {
    safe_name <- paste0(original_safe_name, "_", counter)
    counter <- counter + 1
  }

  safe_name
}

#' Print method for tf_ggplot
#' @param x A tf_ggplot object
#' @param ... Additional arguments
#' @returns `x`, invisibly. Called for its side effect of printing the plot.
#' @export
print.tf_ggplot <- function(x, ...) {
  # If there are tf layers or tf aesthetics, finalize before printing
  all_layers <- attr(x, "all_layers")
  if (
    (!is.null(all_layers) && length(all_layers) > 0) ||
      length(parse_tf_aesthetics(x$mapping, x$data)$tf_aes) > 0 ||
      length(parse_tf_aesthetics(x$mapping, x$data)$scalar_tf_aes) > 0
  ) {
    regular_plot <- finalize_tf_ggplot(x)
    print(regular_plot)
  } else {
    # No tf aesthetics, print as regular ggplot
    class(x) <- setdiff(class(x), "tf_ggplot")
    print(x)
  }
}

#' ggplot_build method for tf_ggplot
#' @param plot A tf_ggplot object
#' @param ... Additional arguments passed through from [ggplot2::ggplot_build()].
#' @returns A built ggplot object (class `ggplot_built`).
#' @export
ggplot_build.tf_ggplot <- function(plot, ...) {
  # Finalize tf_ggplot before building
  all_layers <- attr(plot, "all_layers")
  if (
    (!is.null(all_layers) && length(all_layers) > 0) ||
      length(parse_tf_aesthetics(plot$mapping, plot$data)$tf_aes) > 0 ||
      length(parse_tf_aesthetics(plot$mapping, plot$data)$scalar_tf_aes) > 0
  ) {
    regular_plot <- finalize_tf_ggplot(plot)
    return(ggplot_build(regular_plot))
  } else {
    # No tf aesthetics, build as regular ggplot
    class(plot) <- setdiff(class(plot), "tf_ggplot")
    return(ggplot_build(plot))
  }
}

# Register tf aesthetics as optional on ggplot2 geoms so that
# geom_line(aes(tf = ...)) etc. do not warn "Ignoring unknown aesthetics".
# Appending to optional_aes is the lightest-touch approach: it prevents the
# warning at the source (inside ggplot2::layer()) and works in all contexts
# (interactive, knitr, testthat). Cleaned up in .onUnload().
.tf_optional_aes <- c("tf", "tf_x", "tf_y", "tf_ymin", "tf_ymax")

.onLoad <- function(libname, pkgname) {
  geoms <- list(
    ggplot2::GeomLine,
    ggplot2::GeomPath,
    ggplot2::GeomPoint,
    ggplot2::GeomRibbon,
    ggplot2::GeomArea,
    ggplot2::GeomStep
  )
  for (g in geoms) {
    g$optional_aes <- union(g$optional_aes, .tf_optional_aes)
  }
}

.onUnload <- function(libpath) {
  geoms <- list(
    ggplot2::GeomLine,
    ggplot2::GeomPath,
    ggplot2::GeomPoint,
    ggplot2::GeomRibbon,
    ggplot2::GeomArea,
    ggplot2::GeomStep
  )
  for (g in geoms) {
    g$optional_aes <- setdiff(g$optional_aes, .tf_optional_aes)
  }
}
