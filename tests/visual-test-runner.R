#!/usr/bin/env Rscript

# Visual Test Runner for tf_ggplot
# This script helps run visual tests and organize the output plots

# Load required packages
library(testthat)
if (!require(ggplot2, quietly = TRUE)) {
  stop("ggplot2 is required for visual tests")
}

# Source the package functions (assuming we're in package root)
if (file.exists("R")) {
  source_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
  invisible(lapply(source_files, source))
} else {
  cat(
    "Warning: R/ directory not found. Make sure tf_ggplot functions are loaded.\n"
  )
}

# Create output directory structure
setup_visual_test_environment <- function() {
  base_dir <- file.path("tests", "visual-output")

  # Create subdirectories for organization
  dirs <- c(
    file.path(base_dir, "plots"), # Individual plot files
    file.path(base_dir, "comparisons"), # Side-by-side comparisons
    file.path(base_dir, "reports") # HTML reports
  )

  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      cat("Created directory:", dir, "\n")
    }
  }

  return(base_dir)
}

# Enhanced plot saving with metadata
save_visual_test_plot <- function(
  plot,
  name,
  category = "general",
  width = 8,
  height = 6,
  dpi = 150,
  description = NULL
) {
  base_dir <- setup_visual_test_environment()

  # Save the plot
  filename <- paste0(
    sprintf(
      "%02d",
      match(
        category,
        c("basic", "advanced", "edge-cases", "comparisons", "general")
      )
    ),
    "_",
    name,
    ".png"
  )
  filepath <- file.path(base_dir, "plots", filename)

  tryCatch(
    {
      ggsave(filepath, plot, width = width, height = height, dpi = dpi)

      # Save metadata
      metadata <- list(
        filename = filename,
        category = category,
        description = description %||% name,
        created = Sys.time(),
        dimensions = c(width = width, height = height),
        dpi = dpi
      )

      metadata_file <- file.path(
        base_dir,
        "plots",
        paste0(tools::file_path_sans_ext(filename), "_meta.rds")
      )
      saveRDS(metadata, metadata_file)

      cat("✓ Saved:", filename, "\n")
      return(filepath)
    },
    error = function(e) {
      cat("✗ Failed to save", filename, ":", e$message, "\n")
      return(NULL)
    }
  )
}

# Create HTML report with all plots
create_visual_report <- function() {
  base_dir <- setup_visual_test_environment()
  plots_dir <- file.path(base_dir, "plots")

  # Get all plot files
  plot_files <- list.files(plots_dir, pattern = "\\.png$", full.names = FALSE)
  plot_files <- sort(plot_files)

  if (length(plot_files) == 0) {
    cat("No plots found to include in report.\n")
    return(NULL)
  }

  # Create HTML content
  html_content <- c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "<title>tf_ggplot Visual Test Report</title>",
    "<style>",
    "body { font-family: Arial, sans-serif; margin: 40px; }",
    ".plot-container { margin: 20px 0; border: 1px solid #ddd; padding: 20px; }",
    ".plot-title { font-size: 18px; font-weight: bold; margin-bottom: 10px; }",
    ".plot-description { color: #666; margin-bottom: 15px; }",
    ".plot-image { max-width: 100%; height: auto; border: 1px solid #ccc; }",
    ".metadata { font-size: 12px; color: #888; margin-top: 10px; }",
    ".category-header { font-size: 24px; color: #333; margin: 30px 0 15px 0; border-bottom: 2px solid #333; }",
    "</style>",
    "</head>",
    "<body>",
    "<h1>tf_ggplot Visual Test Report</h1>",
    paste0("<p>Generated on: ", Sys.time(), "</p>"),
    paste0("<p>Total plots: ", length(plot_files), "</p>")
  )

  # Group plots by category
  categories <- list()
  for (file in plot_files) {
    # Extract category from filename prefix
    category_num <- substr(file, 1, 2)
    category_name <- switch(
      category_num,
      "01" = "Basic Functionality",
      "02" = "Basic Functionality",
      "03" = "Advanced Features",
      "04" = "Advanced Features",
      "05" = "Advanced Features",
      "06" = "Advanced Features",
      "07" = "Comparisons",
      "08" = "Comparisons",
      "09" = "Geom Types",
      "10" = "Geom Types",
      "11" = "Geom Types",
      "12" = "Geom Types",
      "13" = "Geom Types",
      "14" = "Edge Cases",
      "15" = "Edge Cases",
      "16" = "Edge Cases",
      "17" = "Edge Cases",
      "General"
    )

    if (is.null(categories[[category_name]])) {
      categories[[category_name]] <- character(0)
    }
    categories[[category_name]] <- c(categories[[category_name]], file)
  }

  # Add plots to HTML, grouped by category
  for (category in names(categories)) {
    html_content <- c(
      html_content,
      paste0('<div class="category-header">', category, '</div>')
    )

    for (file in categories[[category]]) {
      # Try to load metadata
      meta_file <- file.path(
        plots_dir,
        paste0(tools::file_path_sans_ext(file), "_meta.rds")
      )

      description <- "No description available"
      metadata_info <- ""

      if (file.exists(meta_file)) {
        tryCatch(
          {
            meta <- readRDS(meta_file)
            description <- meta$description
            metadata_info <- paste0(
              "Size: ",
              meta$dimensions["width"],
              "×",
              meta$dimensions["height"],
              " | DPI: ",
              meta$dpi,
              " | Created: ",
              format(meta$created, "%Y-%m-%d %H:%M")
            )
          },
          error = function(e) {
            # Ignore metadata errors
          }
        )
      }

      plot_html <- c(
        '<div class="plot-container">',
        paste0(
          '<div class="plot-title">',
          tools::file_path_sans_ext(file),
          '</div>'
        ),
        paste0('<div class="plot-description">', description, '</div>'),
        paste0(
          '<img src="plots/',
          file,
          '" class="plot-image" alt="',
          file,
          '">'
        ),
        paste0('<div class="metadata">', metadata_info, '</div>'),
        '</div>'
      )

      html_content <- c(html_content, plot_html)
    }
  }

  # Close HTML
  html_content <- c(html_content, "</body>", "</html>")

  # Write report
  report_file <- file.path(base_dir, "reports", "visual_test_report.html")
  writeLines(html_content, report_file)

  cat("✓ Visual report created:", report_file, "\n")
  cat("Open in browser to view all test plots.\n")

  return(report_file)
}

# Main function to run all visual tests
run_tf_ggplot_visual_tests <- function(
  interactive_display = FALSE,
  create_report = TRUE,
  open_report = interactive()
) {
  cat("=== tf_ggplot Visual Test Suite ===\n\n")

  # Setup environment
  base_dir <- setup_visual_test_environment()

  # Override the save function in the test environment
  assign(
    "save_test_plot",
    function(
      plot,
      filename,
      width = 8,
      height = 6,
      display = interactive_display,
      save_plot = TRUE
    ) {
      if (save_plot) {
        save_visual_test_plot(plot, filename, width = width, height = height)
      }
      if (display) {
        print(plot)
      }
      invisible(plot)
    },
    envir = .GlobalEnv
  )

  # Run the visual tests
  cat("Running visual tests...\n")
  tryCatch(
    {
      test_file(file.path("tests", "testthat", "test-tf-ggplot-visual.R"))
      cat("✓ All visual tests completed successfully!\n\n")
    },
    error = function(e) {
      cat("✗ Error in visual tests:", e$message, "\n\n")
      return(FALSE)
    }
  )

  # Create HTML report
  if (create_report) {
    cat("Creating visual report...\n")
    report_file <- create_visual_report()

    if (open_report && !is.null(report_file) && file.exists(report_file)) {
      if (.Platform$OS.type == "windows") {
        shell.exec(report_file)
      } else {
        system(paste("open", shQuote(report_file)), ignore.stderr = TRUE)
      }
    }
  }

  cat("\nVisual testing complete!\n")
  cat("Results in:", base_dir, "\n")

  return(TRUE)
}

# Quick comparison function
compare_with_current <- function() {
  cat("Creating comparison plots: tf_ggplot vs current geom_spaghetti...\n")

  # This would create side-by-side comparisons
  # Implementation depends on having both systems available

  cat("Comparison plots would be created here.\n")
}

# Print usage information
print_usage <- function() {
  cat("\n=== tf_ggplot Visual Test Runner ===\n\n")
  cat("Usage:\n")
  cat("  source('tests/visual-test-runner.R')\n")
  cat("  run_tf_ggplot_visual_tests()    # Run all tests, create report\n")
  cat(
    "  create_visual_report()          # Create HTML report from existing plots\n"
  )
  cat("  compare_with_current()          # Compare with geom_spaghetti\n\n")
  cat("Options:\n")
  cat("  interactive_display = TRUE      # Show plots as they're created\n")
  cat("  create_report = FALSE           # Skip HTML report generation\n")
  cat("  open_report = FALSE             # Don't auto-open the report\n\n")
  cat("Output directories:\n")
  cat("  tests/visual-output/plots/      # Individual plot files\n")
  cat("  tests/visual-output/reports/    # HTML reports\n")
  cat("  tests/visual-output/comparisons/ # Side-by-side comparisons\n\n")
}

# Show usage when sourced
if (interactive()) {
  print_usage()
}
