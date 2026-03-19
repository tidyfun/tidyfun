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
#'   grid? Defaults to TRUE.
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
  interpolate = TRUE
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

  # Create base ggplot object
  p <- ggplot(data = data, mapping = mapping, ...)

  # Convert to tf_ggplot class and add tf-specific attributes
  class(p) <- c("tf_ggplot", class(p))
  attr(p, "tf_arg") <- arg
  attr(p, "tf_interpolate") <- interpolate
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

      # Set the layer to use original data
      e2$data <- e1$data

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
  # Does NOT include x/y/group from tf aes — those are per-layer.
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
      # fall back to plot-level enriched_data.  Never use layer$data here —
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
          interpolate = interpolate
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
  interpolate
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
  # user_arg can be NULL, a numeric vector (explicit points), or a single integer
  # (desired grid length, resolved to an equidistant grid over the tf domain).
  arg <- user_arg
  if (is.null(arg)) {
    first_arg <- tf_arg(tf_objects[[1]])
    arg <- if (is.list(first_arg)) first_arg[[1]] else first_arg
  } else if (length(arg) == 1L) {
    domain <- tf_domain(tf_objects[[1]])
    arg <- seq(domain[1], domain[2], length.out = as.integer(arg))
  }

  # If there is no plot-level data (e.g. tf_ggplot() + geom_line(aes(tf = tf_rgp(5)))),
  # synthesise a one-row-per-function data frame so joins and indexing work correctly.
  n_funcs <- common_n_funcs
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
      n_rows <- 1L
    } else if (n_rows == 1L && n_funcs > 1L) {
      enriched_data <- enriched_data[rep(1L, n_funcs), , drop = FALSE]
      n_rows <- n_funcs
    } else {
      cli::cli_abort(c(
        "Layer data cannot be aligned with the evaluated tf aesthetics",
        "i" = "Data has {n_rows} row(s), but tf aesthetics evaluate to {n_funcs} function(s)."
      ))
    }
  }
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
      g_col
    )
  }

  # Layer-level scalar tf aes: column is already in long_data (joined from enriched_data)
  for (aes_name in names(parsed_aes$scalar_tf_aes)) {
    key <- paste0(aes_name, ".layer.", layer_idx)
    if (key %in% names(scalar_col_map)) {
      new_mapping[[aes_name]] <- rlang::sym(scalar_col_map[[key]])
    }
  }

  list(long_data = long_data, new_mapping = new_mapping)
}

#' Add a tf aesthetic to an ggplot2 mapping object
#' @keywords internal
add_tf_aes_to_mapping <- function(mapping, aes_name, val_col, arg_col, id_col) {
  if (aes_name %in% c("tf", "tf_y")) {
    mapping$y <- rlang::sym(val_col)
    mapping$x <- rlang::sym(arg_col)
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

# Muffle ggplot2's "Ignoring unknown aesthetics" warnings when all unknown
# aesthetics are tf-specific. This avoids monkey-patching ggplot2 internals.
.tidyfun_aes_warning_handler <- function(w) {
  # Strip ANSI escape codes (cli adds them in interactive/color-capable sessions)
  msg <- cli::ansi_strip(conditionMessage(w))
  if (startsWith(msg, "Ignoring unknown aesthetics:")) {
    aes_str <- sub("^Ignoring unknown aesthetics:\\s*", "", msg)
    # Split on commas, "and", or ", and" (Oxford comma); drop empty fragments
    aes_names <- trimws(strsplit(aes_str, ",\\s*and\\s+|,\\s*|\\s+and\\s+")[[
      1
    ]])
    aes_names <- aes_names[nzchar(aes_names)]
    tf_aes <- c("tf", "tf_x", "tf_y", "tf_ymin", "tf_ymax")
    if (length(aes_names) > 0L && all(aes_names %in% tf_aes)) {
      tryInvokeRestart("muffleWarning")
    }
  }
}

.onLoad <- function(libname, pkgname) {
  # globalCallingHandlers() errors when called with handlers on the stack
  # (e.g., during R CMD INSTALL). Fail silently; the handler is a convenience
  # that suppresses cosmetic warnings, not a correctness requirement.
  tryCatch(
    globalCallingHandlers(warning = .tidyfun_aes_warning_handler),
    error = function(e) NULL
  )
}

.onUnload <- function(libpath) {
  # globalCallingHandlers() errors when called with handlers on the stack
  # (e.g., during R CMD check). Fail silently — the handler becomes inert
  # once the package namespace is unloaded anyway.
  tryCatch(
    suspendInterrupts({
      # Snapshot and clear all global handlers, then re-register all except ours.
      # Re-register in reverse order to preserve original precedence (handlers
      # are returned most-recent-first by globalCallingHandlers()).
      handlers <- globalCallingHandlers()
      globalCallingHandlers(NULL)
      for (i in rev(seq_along(handlers))) {
        if (!identical(handlers[[i]], .tidyfun_aes_warning_handler)) {
          args <- list(handlers[[i]])
          names(args) <- names(handlers)[i]
          do.call(globalCallingHandlers, args)
        }
      }
    }),
    error = function(e) NULL
  )
}
