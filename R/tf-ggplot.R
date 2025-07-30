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
  # If e2 is a layer with aesthetics, we may need to convert tf_ggplot
  if (inherits(e2, "LayerInstance") || inherits(e2, "Layer")) {
    # Extract layer mapping
    layer_mapping <- e2$mapping %||% aes()

    # Check if layer has tf aesthetics
    parsed_layer_aes <- parse_tf_aesthetics(layer_mapping, e1$data)
    parsed_plot_aes <- parse_tf_aesthetics(e1$mapping, e1$data)

    has_tf_aes <- length(parsed_layer_aes$tf_aes) > 0 ||
      length(parsed_plot_aes$tf_aes) > 0

    if (has_tf_aes) {
      # Convert tf_ggplot to regular ggplot with transformed data
      regular_plot <- convert_tf_ggplot(e1, layer_mapping)

      # Update layer mapping to use transformed columns
      if (length(parsed_layer_aes$tf_aes) > 0) {
        # Need to update layer's mapping to reference transformed columns
        transformed_mapping <- parsed_layer_aes$regular_aes

        # Add the tf->x,y,group mappings that were created in convert_tf_ggplot
        for (tf_aes_name in names(parsed_layer_aes$tf_aes)) {
          tf_expr <- quo_get_expr(parsed_layer_aes$tf_aes[[tf_aes_name]])
          if (is.symbol(tf_expr)) {
            tf_col_name <- as.character(tf_expr)
          } else {
            # Complex expression - use generated name
            tf_col_name <- paste0(
              ".tf_expr_",
              which(names(parsed_layer_aes$tf_aes) == tf_aes_name)
            )
          }

          if (tf_aes_name == "tf" || tf_aes_name == "tf_y") {
            transformed_mapping$x <- sym(paste0(tf_col_name, ".arg"))
            transformed_mapping$y <- sym(paste0(tf_col_name, ".value"))
            transformed_mapping$group <- sym(paste0(tf_col_name, ".id"))
          }
          # Add other tf aesthetic mappings as needed
        }

        e2$mapping <- transformed_mapping
      }

      # Add layer to converted plot
      result <- regular_plot + e2
      return(result)
    }
  }

  # No tf aesthetics involved - use standard ggplot addition
  # But first convert to regular ggplot to avoid infinite recursion
  regular_plot <- ggplot(data = e1$data, mapping = e1$mapping)
  regular_plot$theme <- e1$theme
  regular_plot$coordinates <- e1$coordinates
  regular_plot$facet <- e1$facet
  regular_plot$labels <- e1$labels

  return(regular_plot + e2)
}

# Helper function to check if mapping is an aes object
is_mapping <- function(x) {
  inherits(x, "uneval")
}
