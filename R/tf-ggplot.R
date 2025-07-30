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
#' @param arg Optional. Evaluation grid for tf objects. If not provided, uses
#'   the default evaluation grid of the tf objects.
#' @param interpolate Logical. Should tf objects be interpolated to the evaluation
#'   grid? Defaults to TRUE.
#' @param ... Other arguments passed to ggplot2 functions.
#'
#' @details
#' tf_ggplot supports the following tf-specific aesthetics:
#' - `tf`: Maps a tf object to y aesthetic (shorthand for tf_y)
#' - `tf_x`: Maps a tf object to x aesthetic
#' - `tf_y`: Maps a tf object to y aesthetic
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
#' \dontrun{
#' # Basic usage
#' data <- data.frame(id = 1:3, group = c("A", "A", "B"))
#' data$func <- tf_rgp(3)
#'
#' # Method 1: tf aesthetic in constructor
#' tf_ggplot(data, aes(tf = func, color = group)) + geom_line()
#'
#' # Method 2: tf aesthetic in geom (equivalent)
#' tf_ggplot(data) + geom_line(aes(tf = func, color = group))
#'
#' # Confidence bands
#' tf_ggplot(data) +
#'   geom_ribbon(aes(tf_ymin = lower, tf_ymax = upper), alpha = 0.3) +
#'   geom_line(aes(tf = mean_func))
#' }
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

  # Create base ggplot object
  p <- ggplot(data = data, mapping = mapping, ...)

  # Convert to tf_ggplot class and add tf-specific attributes
  class(p) <- c("tf_ggplot", class(p))
  attr(p, "tf_arg") <- arg
  attr(p, "tf_interpolate") <- interpolate
  attr(p, "tf_original_data") <- data
  attr(p, "tf_original_mapping") <- mapping
  attr(p, "tf_layers") <- list() # Store layers with tf aesthetics
  attr(p, "tf_expression_counter") <- 0 # Counter for unique expression names

  return(p)
}

#' Check if object is a tf_ggplot
#' @param x Object to test
#' @export
is_tf_ggplot <- function(x) {
  inherits(x, "tf_ggplot")
}

#' Parse aesthetic mappings to separate tf and regular aesthetics
#'
#' @param mapping An aesthetic mapping created with aes()
#' @param data Data frame to evaluate expressions against
#' @return List with tf_aes, scalar_tf_aes, and regular_aes components
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
        expr <- rlang::quo_get_expr(mapping[[i]])
        # Check if expression contains tf function calls
        expr_text <- rlang::expr_deparse(expr)
        if (grepl("tf_", expr_text)) {
          is_scalar_tf_aes[i] <- TRUE
        }
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

#' Transform data with tf aesthetics into long format
#'
#' @param data Data frame containing tf objects
#' @param tf_aesthetics List of tf aesthetic mappings
#' @param arg Optional evaluation grid
#' @param interpolate Whether to interpolate tf objects
#' @return Transformed data frame in long format
#' @export
transform_tf_data <- function(
  data,
  tf_aesthetics,
  arg = NULL,
  interpolate = TRUE
) {
  if (length(tf_aesthetics) == 0) {
    return(data)
  }

  # Check for large data expansions
  n_rows <- nrow(data)
  n_tf_cols <- length(tf_aesthetics)

  # Estimate expansion - get first tf column to check grid size
  first_tf_expr <- quo_get_expr(tf_aesthetics[[1]])
  if (is.symbol(first_tf_expr)) {
    first_tf_name <- as.character(first_tf_expr)
  } else {
    first_tf_name <- NULL
  }

  if (!is.null(first_tf_name) && first_tf_name %in% names(data)) {
    first_tf <- data[[first_tf_name]]
    if (is_tf(first_tf)) {
      if (is.null(arg)) {
        first_tf_arg <- tf_arg(first_tf)
        if (is.list(first_tf_arg)) {
          arg_length <- length(first_tf_arg[[1]])
        } else {
          arg_length <- length(first_tf_arg)
        }
      } else {
        arg_length <- length(arg)
      }

      estimated_rows <- n_rows * arg_length

      # Warn about large expansions
      if (estimated_rows > 10000) {
        cli::cli_warn(c(
          "Large data expansion detected: {n_rows} rows -> {estimated_rows} rows",
          "i" = "Consider using a coarser evaluation grid with the 'arg' parameter",
          "i" = "This may impact memory usage and plotting performance"
        ))
      }
    }
  }

  # Extract tf column names from aesthetic expressions
  tf_col_names <- character(length(tf_aesthetics))
  names(tf_col_names) <- names(tf_aesthetics)

  for (i in seq_along(tf_aesthetics)) {
    # Extract column name from quosure
    expr <- quo_get_expr(tf_aesthetics[[i]])
    if (is.symbol(expr)) {
      # Simple column reference
      tf_col_names[i] <- as.character(expr)
    } else {
      # Complex expression - evaluate it and store result
      tf_col_names[i] <- paste0(".tf_expr_", i)
    }
  }

  # Evaluate complex expressions and validate tf objects
  for (i in seq_along(tf_aesthetics)) {
    col_name <- tf_col_names[i]
    expr <- quo_get_expr(tf_aesthetics[[i]])

    if (is.symbol(expr)) {
      # Simple column reference - validate it exists and is tf
      if (!col_name %in% names(data)) {
        cli::cli_abort("Column {.val {col_name}} not found in data")
      }
      if (!is_tf(data[[col_name]])) {
        cli::cli_abort(
          "Column {.val {col_name}} must be a tf object, not {.obj_type_friendly {data[[col_name]]}}"
        )
      }
    } else {
      # Complex expression - evaluate it and validate result is tf
      tryCatch(
        {
          result <- rlang::eval_tidy(tf_aesthetics[[i]], data = data)
          if (!is_tf(result)) {
            cli::cli_abort(
              "tf aesthetic expression {.code {rlang::expr_deparse(expr)}} must evaluate to a tf object, not {.obj_type_friendly {result}}"
            )
          }
          # Store the evaluated result in data for later use
          data[[col_name]] <- result
        },
        error = function(e) {
          cli::cli_abort(
            "Error evaluating tf aesthetic expression {.code {rlang::expr_deparse(expr)}}: {e$message}"
          )
        }
      )
    }
  }

  # Determine common evaluation grid
  if (is.null(arg)) {
    # Use the first tf object's grid as default
    first_tf <- data[[tf_col_names[1]]]
    first_tf_arg <- tf_arg(first_tf)
    if (is.list(first_tf_arg)) {
      arg <- first_tf_arg[[1]]
    } else {
      arg <- first_tf_arg
    }
  }

  # For simplicity, let's handle one tf aesthetic at a time
  # This can be optimized later for multiple tf aesthetics

  if (length(tf_col_names) > 1) {
    cli::cli_warn(
      "Multiple tf aesthetics not fully supported yet. Using first tf aesthetic only."
    )
  }

  # Get the first (and for now, only) tf column
  first_tf_aes <- names(tf_col_names)[1]
  first_tf_col <- tf_col_names[1]
  tf_obj <- data[[first_tf_col]]

  # Use tf_unnest to convert to long format
  temp_data <- data
  temp_data$.row_id <- seq_len(nrow(data))

  # Use tf_unnest.tf directly on the tf object
  tf_long <- tf_unnest(tf_obj, arg = arg, interpolate = interpolate)

  # Check for large data expansion and warn
  n_rows_original <- nrow(data)
  n_rows_expanded <- nrow(tf_long)
  expansion_factor <- n_rows_expanded / n_rows_original

  if (n_rows_expanded > 500 || expansion_factor > 20) {
    cli::cli_warn(
      "Large data expansion: {n_rows_original} rows expanded to {n_rows_expanded} rows (factor of {round(expansion_factor)}). This may impact memory usage and performance."
    )
  }

  # Add row identifiers to match back to original data
  tf_long$.row_id <- rep(seq_len(nrow(data)), each = length(arg))

  # Create proper column names
  arg_col <- paste0(first_tf_col, ".arg")
  value_col <- paste0(first_tf_col, ".value")
  id_col <- paste0(first_tf_col, ".id")

  # Rename columns appropriately
  if ("arg" %in% names(tf_long)) {
    names(tf_long)[names(tf_long) == "arg"] <- arg_col
  }
  if ("data" %in% names(tf_long)) {
    names(tf_long)[names(tf_long) == "data"] <- value_col
  } else if ("value" %in% names(tf_long)) {
    names(tf_long)[names(tf_long) == "value"] <- value_col
  }
  if ("id" %in% names(tf_long)) {
    names(tf_long)[names(tf_long) == "id"] <- id_col
  }

  # Join with original data to replicate non-tf columns
  transformed_data <- tf_long |>
    left_join(temp_data, by = ".row_id") |>
    select(-!!sym(first_tf_col), -.row_id) # Remove original tf column and temp id

  # Convert func.id back to numeric for consistency
  if (paste0(first_tf_col, ".id") %in% names(transformed_data)) {
    id_col <- paste0(first_tf_col, ".id")
    transformed_data[[id_col]] <- as.numeric(as.character(transformed_data[[
      id_col
    ]]))
  }

  return(transformed_data)
}

#' Convert tf_ggplot object to regular ggplot with transformed data
#'
#' @param tf_plot A tf_ggplot object
#' @param layer_mapping Additional aesthetic mapping from a layer
#' @return Regular ggplot object with transformed data
#' @keywords internal
convert_tf_ggplot <- function(tf_plot, layer_mapping = aes()) {
  # Combine plot-level and layer-level mappings
  # Layer mapping takes precedence over plot mapping
  combined_mapping <- modifyList(tf_plot$mapping, layer_mapping)

  # Parse tf aesthetics
  parsed_aes <- parse_tf_aesthetics(combined_mapping, tf_plot$data)

  if (length(parsed_aes$tf_aes) == 0 && length(parsed_aes$scalar_tf_aes) == 0) {
    # No tf aesthetics - return regular ggplot
    return(ggplot(data = tf_plot$data, mapping = combined_mapping))
  }

  # Evaluate scalar tf aesthetics first
  data_with_scalars <- tf_plot$data
  if (length(parsed_aes$scalar_tf_aes) > 0) {
    for (i in seq_along(parsed_aes$scalar_tf_aes)) {
      aes_name <- names(parsed_aes$scalar_tf_aes)[i]
      aes_quo <- parsed_aes$scalar_tf_aes[[i]]

      # Evaluate the scalar tf expression
      tryCatch(
        {
          result <- rlang::eval_tidy(aes_quo, data = data_with_scalars)
          # Add result as a new column
          data_with_scalars[[paste0(".scalar_", aes_name)]] <- result
        },
        error = function(e) {
          cli::cli_abort(
            "Error evaluating scalar tf aesthetic {.field {aes_name}}: {e$message}"
          )
        }
      )
    }
  }

  # Transform data if there are tf aesthetics
  if (length(parsed_aes$tf_aes) > 0) {
    transformed_data <- transform_tf_data(
      data = data_with_scalars,
      tf_aesthetics = parsed_aes$tf_aes,
      arg = attr(tf_plot, "tf_arg"),
      interpolate = attr(tf_plot, "tf_interpolate")
    )
  } else {
    # No tf aesthetics, just use data with scalar evaluations
    transformed_data <- data_with_scalars
  }

  # Create new aesthetic mapping for transformed data
  new_mapping <- parsed_aes$regular_aes

  # Add scalar tf aesthetics to regular mapping with evaluated column names
  for (i in seq_along(parsed_aes$scalar_tf_aes)) {
    aes_name <- names(parsed_aes$scalar_tf_aes)[i]
    new_mapping[[aes_name]] <- sym(paste0(".scalar_", aes_name))
  }

  for (tf_aes_name in names(parsed_aes$tf_aes)) {
    # Get the tf column name (from transform_tf_data)
    tf_expr <- quo_get_expr(parsed_aes$tf_aes[[tf_aes_name]])
    if (is.symbol(tf_expr)) {
      tf_col_name <- as.character(tf_expr)
    } else {
      # Complex expression - use generated name
      tf_col_name <- paste0(
        ".tf_expr_",
        which(names(parsed_aes$tf_aes) == tf_aes_name)
      )
    }

    # Map tf aesthetic to regular ggplot aesthetic
    if (tf_aes_name == "tf" || tf_aes_name == "tf_y") {
      new_mapping$y <- sym(paste0(tf_col_name, ".value"))
      new_mapping$x <- sym(paste0(tf_col_name, ".arg"))
      new_mapping$group <- sym(paste0(tf_col_name, ".id"))
    } else if (tf_aes_name == "tf_x") {
      new_mapping$x <- sym(paste0(tf_col_name, ".value"))
      # For tf_x, we need a group based on the tf_x object
      if (is.null(new_mapping$group)) {
        new_mapping$group <- sym(paste0(tf_col_name, ".id"))
      }
    } else if (tf_aes_name == "tf_ymin") {
      new_mapping$ymin <- sym(paste0(tf_col_name, ".value"))
      if (is.null(new_mapping$x)) {
        new_mapping$x <- sym(paste0(tf_col_name, ".arg"))
      }
      if (is.null(new_mapping$group)) {
        new_mapping$group <- sym(paste0(tf_col_name, ".id"))
      }
    } else if (tf_aes_name == "tf_ymax") {
      new_mapping$ymax <- sym(paste0(tf_col_name, ".value"))
      if (is.null(new_mapping$x)) {
        new_mapping$x <- sym(paste0(tf_col_name, ".arg"))
      }
      if (is.null(new_mapping$group)) {
        new_mapping$group <- sym(paste0(tf_col_name, ".id"))
      }
    }
  }

  # Create regular ggplot with transformed data
  regular_plot <- ggplot(data = transformed_data, mapping = new_mapping)

  # Copy over plot properties
  regular_plot$theme <- tf_plot$theme
  regular_plot$coordinates <- tf_plot$coordinates
  regular_plot$facet <- tf_plot$facet
  regular_plot$labels <- tf_plot$labels

  return(regular_plot)
}

#' Add layers to tf_ggplot objects
#'
#' @param e1 A tf_ggplot object
#' @param e2 A ggplot2 layer, scale, theme, etc.
#' @export
`+.tf_ggplot` <- function(e1, e2) {
  # Debug output
  # cat("DEBUG: +.tf_ggplot called\n")

  # If e2 is a layer, check if it has tf aesthetics
  if (inherits(e2, "LayerInstance") || inherits(e2, "Layer")) {
    # Extract layer mapping
    layer_mapping <- e2$mapping %||% aes()

    # Check if this layer or the plot has tf aesthetics
    parsed_layer_aes <- parse_tf_aesthetics(layer_mapping, e1$data)
    parsed_plot_aes <- parse_tf_aesthetics(e1$mapping, e1$data)

    layer_has_tf <- length(parsed_layer_aes$tf_aes) > 0 ||
      length(parsed_layer_aes$scalar_tf_aes) > 0
    plot_has_tf <- length(parsed_plot_aes$tf_aes) > 0 ||
      length(parsed_plot_aes$scalar_tf_aes) > 0

    # cat("DEBUG: layer_has_tf =", layer_has_tf, ", plot_has_tf =", plot_has_tf, "\n")
    # cat("DEBUG: layer tf_aes =", length(parsed_layer_aes$tf_aes), ", plot tf_aes =", length(parsed_plot_aes$tf_aes), "\n")

    if (layer_has_tf || plot_has_tf) {
      # cat("DEBUG: Taking tf path - storing layer\n")
      # Store this layer with tf aesthetics
      tf_layers <- attr(e1, "tf_layers")
      tf_layers[[length(tf_layers) + 1]] <- list(
        layer = e2,
        layer_mapping = layer_mapping,
        parsed_aes = parsed_layer_aes
      )
      attr(e1, "tf_layers") <- tf_layers

      # Return the tf_ggplot object to continue accumulating layers
      return(e1)
    } else {
      # cat("DEBUG: No tf aesthetics found, checking for finalization\n")
    }
  }

  # Non-tf layer or non-layer object - need to finalize tf_ggplot
  if (
    length(attr(e1, "tf_layers")) > 0 ||
      length(parse_tf_aesthetics(e1$mapping, e1$data)$tf_aes) > 0 ||
      length(parse_tf_aesthetics(e1$mapping, e1$data)$scalar_tf_aes) > 0
  ) {
    # cat("DEBUG: Finalizing tf_ggplot\n")
    # Convert tf_ggplot with all accumulated layers
    regular_plot <- finalize_tf_ggplot(e1)
    return(regular_plot + e2)
  }

  # cat("DEBUG: Converting to regular ggplot\n")
  # No tf aesthetics at all - convert to regular ggplot
  regular_plot <- ggplot(data = e1$data, mapping = e1$mapping)
  regular_plot$theme <- e1$theme
  regular_plot$coordinates <- e1$coordinates
  regular_plot$facet <- e1$facet
  regular_plot$labels <- e1$labels

  return(regular_plot + e2)
}

#' Finalize tf_ggplot by processing all tf expressions and creating regular ggplot
#'
#' @param tf_plot A tf_ggplot object with accumulated layers
#' @return Regular ggplot object with all layers properly transformed
#' @keywords internal
finalize_tf_ggplot <- function(tf_plot) {
  # cat("DEBUG: Starting finalize_tf_ggplot\n")
  # Collect all tf expressions from plot and layers
  tf_expressions <- list()
  scalar_tf_expressions <- list()
  expression_counter <- attr(tf_plot, "tf_expression_counter")

  # Parse plot-level aesthetics
  parsed_plot_aes <- parse_tf_aesthetics(tf_plot$mapping, tf_plot$data)

  # Add plot-level tf expressions
  for (aes_name in names(parsed_plot_aes$tf_aes)) {
    # Get the expression and determine naming strategy
    expr <- rlang::quo_get_expr(parsed_plot_aes$tf_aes[[aes_name]])
    expr_text <- rlang::expr_deparse(expr)

    # Use original column name for simple references, generated name for complex expressions
    if (is.symbol(expr)) {
      expr_id <- as.character(expr)
    } else {
      expression_counter <- expression_counter + 1
      expr_id <- paste0(".tf_expr_", expression_counter)
    }

    tf_expressions[[expr_id]] <- list(
      quo = parsed_plot_aes$tf_aes[[aes_name]],
      target_aes = aes_name,
      source = "plot",
      expr_text = expr_text
    )
  }

  # Add plot-level scalar tf expressions
  for (aes_name in names(parsed_plot_aes$scalar_tf_aes)) {
    expr_id <- paste0(".scalar_", aes_name, "_plot")
    scalar_tf_expressions[[expr_id]] <- list(
      quo = parsed_plot_aes$scalar_tf_aes[[aes_name]],
      target_aes = aes_name,
      source = "plot"
    )
  }

  # Add layer-level tf expressions
  tf_layers <- attr(tf_plot, "tf_layers")
  for (i in seq_along(tf_layers)) {
    layer_info <- tf_layers[[i]]
    parsed_aes <- layer_info$parsed_aes

    # Add layer tf expressions
    for (aes_name in names(parsed_aes$tf_aes)) {
      # Get the expression and determine naming strategy
      expr <- rlang::quo_get_expr(parsed_aes$tf_aes[[aes_name]])
      expr_text <- rlang::expr_deparse(expr)

      # Use original column name for simple references, generated name for complex expressions
      if (is.symbol(expr)) {
        expr_id <- as.character(expr)
      } else {
        expression_counter <- expression_counter + 1
        expr_id <- paste0(".tf_expr_", expression_counter)
      }

      # Add layer suffix to avoid conflicts between plot and layer expressions
      if (expr_id %in% names(tf_expressions)) {
        if (is.symbol(expr)) {
          expr_id <- paste0(expr_id, "_layer_", i)
        }
      }

      tf_expressions[[expr_id]] <- list(
        quo = parsed_aes$tf_aes[[aes_name]],
        target_aes = aes_name,
        source = "layer",
        layer_index = i,
        expr_text = expr_text
      )
    }

    # Add layer scalar tf expressions
    for (aes_name in names(parsed_aes$scalar_tf_aes)) {
      expr_id <- paste0(".scalar_", aes_name, "_layer_", i)
      scalar_tf_expressions[[expr_id]] <- list(
        quo = parsed_aes$scalar_tf_aes[[aes_name]],
        target_aes = aes_name,
        source = "layer",
        layer_index = i
      )
    }
  }

  # Transform data with all tf expressions
  transformed_data <- tf_plot$data

  # Create mapping from expr_id to safe column names for later reference
  expr_id_to_safe_name <- list()

  # Evaluate scalar tf expressions first
  for (expr_id in names(scalar_tf_expressions)) {
    expr_info <- scalar_tf_expressions[[expr_id]]
    tryCatch(
      {
        result <- rlang::eval_tidy(expr_info$quo, data = transformed_data)
        transformed_data[[expr_id]] <- result
      },
      error = function(e) {
        cli::cli_abort("Error evaluating scalar tf aesthetic: {e$message}")
      }
    )
  }

  # Evaluate and transform tf expressions
  if (length(tf_expressions) > 0) {
    # Process each tf expression
    for (expr_id in names(tf_expressions)) {
      expr_info <- tf_expressions[[expr_id]]

      # Evaluate the expression
      tf_expr <- rlang::quo_get_expr(expr_info$quo)
      if (is.symbol(tf_expr)) {
        # Simple column reference
        col_name <- as.character(tf_expr)
        if (!col_name %in% names(transformed_data)) {
          cli::cli_abort("Column {.val {col_name}} not found in data")
        }
        tf_obj <- transformed_data[[col_name]]
      } else {
        # Complex expression - evaluate it
        tryCatch(
          {
            tf_obj <- rlang::eval_tidy(expr_info$quo, data = transformed_data)
          },
          error = function(e) {
            cli::cli_abort("Error evaluating tf expression: {e$message}")
          }
        )
      }

      if (!is_tf(tf_obj)) {
        cli::cli_abort("tf aesthetic must evaluate to a tf object")
      }

      # Store the evaluated tf object
      transformed_data[[expr_id]] <- tf_obj
    }

    # Transform data using the first tf expression to get the structure
    first_tf_id <- names(tf_expressions)[1]
    first_tf_obj <- transformed_data[[first_tf_id]]

    # Determine evaluation grid
    arg <- attr(tf_plot, "tf_arg")
    if (is.null(arg)) {
      first_tf_arg <- tf_arg(first_tf_obj)
      if (is.list(first_tf_arg)) {
        arg <- first_tf_arg[[1]]
      } else {
        arg <- first_tf_arg
      }
    }

    # Transform using tf_unnest for the first expression
    temp_data <- transformed_data
    temp_data$.row_id <- seq_len(nrow(transformed_data))

    # Use first tf object for the base transformation
    tf_long <- tf_unnest(
      first_tf_obj,
      arg = arg,
      interpolate = attr(tf_plot, "tf_interpolate")
    )
    tf_long$.row_id <- rep(seq_len(nrow(transformed_data)), each = length(arg))

    # cat("DEBUG: tf_long columns after tf_unnest:", paste(names(tf_long), collapse=", "), "\n")

    # Rename columns for first expression using expression text for meaningful labels
    first_expr_info <- tf_expressions[[first_tf_id]]
    expr_text <- first_expr_info$expr_text

    # Exclude tf columns that will be removed from conflict check
    original_expr <- rlang::quo_get_expr(first_expr_info$quo)
    existing_names_filtered <- names(transformed_data)
    if (is.symbol(original_expr)) {
      original_col_name <- as.character(original_expr)
      existing_names_filtered <- setdiff(
        existing_names_filtered,
        original_col_name
      )
    }

    safe_name <- make_safe_column_name(
      expr_text,
      existing_names = existing_names_filtered
    )

    # Store the mapping for later use
    expr_id_to_safe_name[[first_tf_id]] <- safe_name

    arg_col <- paste0(safe_name, ".arg")
    value_col <- safe_name
    id_col <- paste0(safe_name, ".id")

    # cat("DEBUG: first_tf_id =", first_tf_id, "\n")
    # cat("DEBUG: Renaming columns to:", arg_col, value_col, id_col, "\n")

    if ("arg" %in% names(tf_long))
      names(tf_long)[names(tf_long) == "arg"] <- arg_col
    if ("data" %in% names(tf_long)) {
      names(tf_long)[names(tf_long) == "data"] <- value_col
    } else if ("value" %in% names(tf_long)) {
      names(tf_long)[names(tf_long) == "value"] <- value_col
    }
    if ("id" %in% names(tf_long))
      names(tf_long)[names(tf_long) == "id"] <- id_col

    # Add other tf expressions as additional columns
    if (length(tf_expressions) > 1) {
      # cat("DEBUG: Adding", length(tf_expressions) - 1, "additional tf expressions\n")
      for (i in 2:length(tf_expressions)) {
        expr_id <- names(tf_expressions)[i]
        expr_info <- tf_expressions[[expr_id]]
        tf_obj <- transformed_data[[expr_id]]

        # Evaluate this tf object on the same grid
        tf_vals <- tf_evaluate(tf_obj, arg = arg)

        # Use expression text for meaningful column names
        expr_text_i <- expr_info$expr_text

        # Exclude tf columns that will be removed from conflict check
        original_expr_i <- rlang::quo_get_expr(expr_info$quo)
        existing_names_filtered_i <- c(names(tf_long), names(transformed_data))
        if (is.symbol(original_expr_i)) {
          original_col_name_i <- as.character(original_expr_i)
          existing_names_filtered_i <- setdiff(
            existing_names_filtered_i,
            original_col_name_i
          )
        }

        safe_name_i <- make_safe_column_name(
          expr_text_i,
          existing_names = existing_names_filtered_i
        )

        # Store the mapping for later use
        expr_id_to_safe_name[[expr_id]] <- safe_name_i

        value_col_i <- safe_name_i
        id_col_i <- paste0(safe_name_i, ".id")
        arg_col_i <- paste0(safe_name_i, ".arg")

        # Create value and id columns
        tf_long[[value_col_i]] <- unlist(tf_vals)
        tf_long[[id_col_i]] <- rep(seq_along(tf_vals), each = length(arg))
        # Add arg column for this expression too
        tf_long[[arg_col_i]] <- rep(arg, length(tf_vals))
      }
    }

    # Remove original tf columns from temp_data before join to avoid conflicts
    temp_data_clean <- temp_data
    for (expr_id in names(tf_expressions)) {
      expr_info <- tf_expressions[[expr_id]]
      original_expr <- rlang::quo_get_expr(expr_info$quo)

      # Remove simple column references from temp_data to avoid join conflicts
      if (is.symbol(original_expr)) {
        original_col_name <- as.character(original_expr)
        if (original_col_name %in% names(temp_data_clean)) {
          temp_data_clean <- select(temp_data_clean, -!!sym(original_col_name))
        }
      }

      # Also remove temporary expression columns if they exist
      if (
        expr_id %in% names(temp_data_clean) && grepl("^\\.tf_expr_", expr_id)
      ) {
        temp_data_clean <- select(temp_data_clean, -!!sym(expr_id))
      }
    }

    # Join with cleaned original data
    transformed_data <- tf_long |>
      left_join(temp_data_clean, by = ".row_id") |>
      select(-.row_id)
  }

  # Create regular ggplot with base mapping (non-tf aesthetics)
  base_mapping <- parsed_plot_aes$regular_aes

  # Add scalar tf aesthetics to base mapping
  for (expr_id in names(scalar_tf_expressions)) {
    expr_info <- scalar_tf_expressions[[expr_id]]
    if (expr_info$source == "plot") {
      base_mapping[[expr_info$target_aes]] <- sym(expr_id)
    }
  }

  # Add tf aesthetics to base mapping (only if from plot level)
  for (expr_id in names(tf_expressions)) {
    expr_info <- tf_expressions[[expr_id]]
    if (expr_info$source == "plot") {
      safe_name <- expr_id_to_safe_name[[expr_id]]
      if (expr_info$target_aes == "tf" || expr_info$target_aes == "tf_y") {
        base_mapping$y <- rlang::sym(safe_name)
        base_mapping$x <- rlang::sym(paste0(safe_name, ".arg"))
        base_mapping$group <- rlang::sym(paste0(safe_name, ".id"))
      } else if (expr_info$target_aes == "tf_x") {
        base_mapping$x <- rlang::sym(safe_name)
        if (is.null(base_mapping$group)) {
          base_mapping$group <- rlang::sym(paste0(safe_name, ".id"))
        }
      }
    }
  }

  # Create the base ggplot
  regular_plot <- ggplot(data = transformed_data, mapping = base_mapping)

  # No need for manual label setting - ggplot2 will use column names automatically

  # Add all accumulated layers with updated mappings
  for (i in seq_along(tf_layers)) {
    layer_info <- tf_layers[[i]]
    layer <- layer_info$layer
    parsed_aes <- layer_info$parsed_aes

    # Create new mapping for this layer
    new_mapping <- parsed_aes$regular_aes

    # Add scalar tf aesthetics
    for (expr_id in names(scalar_tf_expressions)) {
      expr_info <- scalar_tf_expressions[[expr_id]]
      if (expr_info$source == "layer" && expr_info$layer_index == i) {
        new_mapping[[expr_info$target_aes]] <- sym(expr_id)
      }
    }

    # Add tf aesthetics
    for (expr_id in names(tf_expressions)) {
      expr_info <- tf_expressions[[expr_id]]
      if (expr_info$source == "layer" && expr_info$layer_index == i) {
        safe_name <- expr_id_to_safe_name[[expr_id]]
        if (expr_info$target_aes == "tf" || expr_info$target_aes == "tf_y") {
          new_mapping$y <- rlang::sym(safe_name)
          new_mapping$x <- rlang::sym(paste0(safe_name, ".arg"))
          new_mapping$group <- rlang::sym(paste0(safe_name, ".id"))
        } else if (expr_info$target_aes == "tf_x") {
          new_mapping$x <- rlang::sym(safe_name)
          if (is.null(new_mapping$group)) {
            new_mapping$group <- rlang::sym(paste0(safe_name, ".id"))
          }
        }
      }
    }

    # Update layer mapping
    layer$mapping <- new_mapping

    # Add layer to plot
    regular_plot <- regular_plot + layer
  }

  # Copy over plot properties
  regular_plot$theme <- tf_plot$theme
  regular_plot$coordinates <- tf_plot$coordinates
  regular_plot$facet <- tf_plot$facet
  regular_plot$labels <- tf_plot$labels

  # Labels are automatically derived from column names - no manual override needed

  return(regular_plot)
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
#' @export
print.tf_ggplot <- function(x, ...) {
  # If there are tf layers or tf aesthetics, finalize before printing
  if (
    length(attr(x, "tf_layers")) > 0 ||
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
#' @export
ggplot_build.tf_ggplot <- function(plot) {
  # Finalize tf_ggplot before building
  if (
    length(attr(plot, "tf_layers")) > 0 ||
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
