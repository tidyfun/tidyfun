# Visual tests for tf_ggplot functionality
# These tests create actual plots that can be inspected visually during development

# Set up visual testing environment
setup_visual_tests <- function() {
  # Create a temporary directory for test plots if it doesn't exist
  test_dir <- file.path(getwd(), "test-plots")
  if (!dir.exists(test_dir)) {
    dir.create(test_dir, recursive = TRUE)
  }
  return(test_dir)
}

# Helper function to save and optionally display plots
save_test_plot <- function(
  plot,
  filename,
  width = 8,
  height = 6,
  display = interactive(),
  save_plot = TRUE
) {
  if (save_plot) {
    test_dir <- setup_visual_tests()
    full_path <- file.path(test_dir, paste0(filename, ".png"))

    tryCatch(
      {
        ggsave(full_path, plot, width = width, height = height, dpi = 150)
        # cat("Plot saved to:", full_path, "\n")
      },
      error = function(e) {
        warning("Could not save plot: ", e$message)
      }
    )
  }

  if (display) {
    print(plot)
  }

  invisible(plot)
}

test_that("basic tf_ggplot line plots look sensible", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  # Create test data with recognizable patterns
  set.seed(123)
  data <- data.frame(
    id = 1:4,
    group = factor(c("Sine", "Cosine", "Linear", "Quadratic"))
  )

  # Create functions with different patterns for visual verification
  arg_vals <- seq(0, 2 * pi, length.out = 51)
  data$func <- tfd(
    list(
      sin(arg_vals), # Sine wave
      cos(arg_vals), # Cosine wave
      arg_vals / pi, # Linear function
      (arg_vals - pi)^2 / pi^2 # Quadratic function
    ),
    arg = arg_vals
  )

  # Test both aesthetic specification methods
  p1 <- tf_ggplot(data, aes(tf = func, color = group)) +
    geom_line(linewidth = 1) +
    labs(
      title = "tf_ggplot: Basic Line Plot (Constructor Aesthetics)",
      subtitle = "Should show: sine, cosine, linear, and quadratic functions"
    ) +
    theme_minimal()

  p2 <- tf_ggplot(data) +
    suppressWarnings(geom_line(aes(tf = func, color = group), linewidth = 1)) +
    labs(
      title = "tf_ggplot: Basic Line Plot (Geom Aesthetics)",
      subtitle = "Should be identical to constructor version"
    ) +
    theme_minimal()

  # Save plots for visual inspection
  save_test_plot(p1, "01_basic_line_constructor")
  save_test_plot(p2, "02_basic_line_geom")

  # Basic validation that plots were created
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")

  # Verify they have the expected structure
  expect_equal(count_plot_groups(p1), 4)
  expect_equal(count_plot_groups(p2), 4)
})

test_that("tf_ggplot with points and lines looks correct", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  # Create sparse irregular data to test points + lines
  set.seed(456)
  data <- data.frame(
    subject = 1:3,
    treatment = factor(c("Control", "Treatment A", "Treatment B"))
  )

  # Create irregular functions (sparse data)
  arg_vals <- sort(runif(15, 0, 10)) # Irregular time points
  data$func <- tfd(
    list(
      exp(-arg_vals / 5) + rnorm(15, 0, 0.1), # Decay + noise
      2 * exp(-arg_vals / 3) + rnorm(15, 0, 0.15), # Different decay
      1.5 * exp(-arg_vals / 4) + rnorm(15, 0, 0.12) # Third decay
    ),
    arg = list(arg_vals, arg_vals, arg_vals)
  )

  p <- tf_ggplot(data, aes(tf = func, color = treatment)) +
    suppressWarnings(geom_line(alpha = 0.7, linewidth = 1)) +
    suppressWarnings(geom_point(linewidth = 2)) +
    labs(
      title = "tf_ggplot: Lines + Points with Irregular Data",
      subtitle = "Should show: 3 decay curves with visible data points"
    ) +
    theme_minimal()

  save_test_plot(p, "03_lines_points_irregular")

  expect_s3_class(p, "ggplot")
  expect_equal(count_plot_groups(p), 3)
})

test_that("tf_ggplot ribbon plots look sensible", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  # Create confidence band data
  set.seed(789)
  data <- data.frame(method = c("Method A", "Method B"))

  arg_vals <- seq(0, 5, length.out = 26)

  # Create mean functions
  mean_vals1 <- 2 * exp(-arg_vals / 2) * cos(arg_vals)
  mean_vals2 <- 1.5 * exp(-arg_vals / 3) * sin(arg_vals * 1.2)

  # Create confidence bands
  sd1 <- 0.2 + 0.1 * arg_vals / 5 # Increasing uncertainty
  sd2 <- 0.15 + 0.05 * arg_vals / 5

  data$mean_func <- tfd(list(mean_vals1, mean_vals2), arg = arg_vals)
  data$lower_func <- tfd(
    list(mean_vals1 - 1.96 * sd1, mean_vals2 - 1.96 * sd2),
    arg = arg_vals
  )
  data$upper_func <- tfd(
    list(mean_vals1 + 1.96 * sd1, mean_vals2 + 1.96 * sd2),
    arg = arg_vals
  )

  p <- tf_ggplot(data, aes(color = method, fill = method)) +
    suppressWarnings(geom_ribbon(
      aes(tf_ymin = lower_func, tf_ymax = upper_func),
      alpha = 0.3
    )) +
    suppressWarnings(geom_line(aes(tf = mean_func), linewidth = 1)) +
    labs(
      title = "tf_ggplot: Confidence Bands",
      subtitle = "Should show: 2 oscillating functions with confidence ribbons"
    ) +
    theme_minimal()

  save_test_plot(p, "04_confidence_ribbons")

  expect_s3_class(p, "ggplot")
  expect_equal(count_plot_groups(p), 2) # Should have 2 methods
})

test_that("tf_ggplot faceting looks correct", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  # Create data for faceted plot
  set.seed(101112)
  data <- expand.grid(
    subject = 1:4,
    condition = c("Baseline", "Treatment"),
    session = c("Pre", "Post")
  )

  # Create different function patterns for each condition
  arg_vals <- seq(0, 10, length.out = 21)
  n_funcs <- nrow(data)

  func_vals <- map(1:n_funcs, function(i) {
    baseline <- sin(arg_vals * 0.5) + rnorm(21, 0, 0.1)
    if (data$condition[i] == "Treatment") {
      # Treatment effect: amplify and shift
      baseline <- baseline * 1.5 + 0.5
    }
    if (data$session[i] == "Post") {
      # Post effect: add upward trend
      baseline <- baseline + arg_vals * 0.05
    }
    baseline
  })

  data$func <- tfd(func_vals, arg = arg_vals)

  p <- tf_ggplot(data, aes(tf = func, color = condition)) +
    suppressWarnings(geom_line(alpha = 0.7)) +
    facet_grid(session ~ condition) +
    labs(
      title = "tf_ggplot: Faceted Plot",
      subtitle = "Should show: Treatment effects across Pre/Post sessions"
    ) +
    theme_minimal()

  save_test_plot(p, "05_faceted_plot", width = 10, height = 8)

  expect_s3_class(p, "ggplot")

  # Check faceting structure
  built <- suppressWarnings(ggplot_build(p))
  expect_equal(length(unique(built$layout$layout$PANEL)), 4) # 2×2 facets
})

test_that("tf_ggplot with multiple aesthetics looks good", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  # Create data with multiple grouping variables
  set.seed(131415)
  data <- expand.grid(
    method = c("A", "B", "C"),
    replicate = 1:3
  )
  data$difficulty <- factor(rep(c("Easy", "Medium", "Hard"), 3))
  data$performance <- runif(9, 0.5, 1.0)

  # Create performance curves over time
  arg_vals <- seq(0, 12, length.out = 25)
  func_vals <- map(1:nrow(data), function(i) {
    # Base learning curve
    base <- data$performance[i] * (1 - exp(-arg_vals / 3))

    # Method effects
    if (data$method[i] == "B") base <- base * 1.1
    if (data$method[i] == "C") base <- base * 0.9

    # Difficulty effects
    if (data$difficulty[i] == "Medium") base <- base * 0.8
    if (data$difficulty[i] == "Hard") base <- base * 0.6

    # Add noise
    base + rnorm(25, 0, 0.05)
  })

  data$learning_curve <- tfd(func_vals, arg = arg_vals)

  p <- tf_ggplot(
    data,
    aes(
      tf = learning_curve,
      color = method,
      linetype = difficulty,
      alpha = performance
    )
  ) +
    suppressWarnings(geom_line(linewidth = 1)) +
    scale_alpha_continuous(range = c(0.4, 1.0)) +
    labs(
      title = "tf_ggplot: Multiple Aesthetics",
      subtitle = "Should show: Learning curves by method, difficulty, and performance"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  save_test_plot(p, "06_multiple_aesthetics", width = 10, height = 8)

  expect_s3_class(p, "ggplot")
  expect_equal(count_plot_groups(p), 9) # 3 methods × 3 replicates
})

test_that("tf_ggplot comparison with current geom_spaghetti", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  # Create comparison data
  set.seed(161718)
  data <- data.frame(
    id = 1:6,
    group = factor(rep(c("Group A", "Group B"), each = 3))
  )
  data$func <- tf_rgp(6, arg = seq(0, 1, length.out = 31))

  # Current approach with geom_spaghetti
  p_current <- ggplot(data, aes(y = func, color = group)) +
    geom_spaghetti(alpha = 0.7) +
    labs(
      title = "Current: geom_spaghetti",
      subtitle = "6 random Gaussian process functions"
    ) +
    theme_minimal()

  # New approach with tf_ggplot
  p_new <- tf_ggplot(data, aes(tf = func, color = group)) +
    suppressWarnings(geom_line(alpha = 0.7)) +
    labs(
      title = "New: tf_ggplot + geom_line",
      subtitle = "Same data, should look identical"
    ) +
    theme_minimal()

  save_test_plot(p_current, "07_current_spaghetti")
  save_test_plot(p_new, "08_new_tf_ggplot")

  # Both should work
  expect_s3_class(p_current, "ggplot")
  expect_s3_class(p_new, "ggplot")

  # Should have same number of groups
  expect_equal(count_plot_groups(p_current), count_plot_groups(p_new))
})

test_that("tf_ggplot with different geom types", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  # Create test data with distinctive patterns
  set.seed(192021)
  data <- data.frame(id = 1:2)

  arg_vals <- seq(0, 4 * pi, length.out = 41)
  data$func <- tfd(
    list(
      sin(arg_vals) + 0.1 * arg_vals, # Sine with trend
      cos(arg_vals) - 0.1 * arg_vals # Cosine with trend
    ),
    arg = arg_vals
  )

  # Test different geom types
  p_line <- tf_ggplot(data) +
    suppressWarnings(geom_line(aes(tf = func), color = "blue", linewidth = 1)) +
    labs(title = "geom_line", subtitle = "Smooth continuous lines") +
    theme_minimal()

  p_point <- tf_ggplot(data) +
    suppressWarnings(geom_point(
      aes(tf = func),
      color = "red",
      linewidth = 1.5,
      alpha = 0.7
    )) +
    labs(title = "geom_point", subtitle = "Individual evaluation points") +
    theme_minimal()

  p_step <- tf_ggplot(data, arg = seq(0, 4 * pi, length.out = 21)) +
    suppressWarnings(geom_step(
      aes(tf = func),
      color = "green",
      linewidth = 1
    )) +
    labs(title = "geom_step", subtitle = "Step function approximation") +
    theme_minimal()

  p_area <- tf_ggplot(data) +
    suppressWarnings(geom_area(aes(tf = func), fill = "purple", alpha = 0.5)) +
    labs(title = "geom_area", subtitle = "Filled area under curves") +
    theme_minimal()

  # Combined plot
  p_combined <- tf_ggplot(data, aes(tf = func, color = factor(id))) +
    suppressWarnings(geom_line(linewidth = 1, alpha = 0.8)) +
    suppressWarnings(geom_point(linewidth = 2)) +
    labs(
      title = "Combined: Lines + Points",
      subtitle = "Should show both continuous lines and data points",
      color = "Function"
    ) +
    theme_minimal()

  save_test_plot(p_line, "09_geom_line")
  save_test_plot(p_point, "10_geom_point")
  save_test_plot(p_step, "11_geom_step")
  save_test_plot(p_area, "12_geom_area")
  save_test_plot(p_combined, "13_combined_geoms")

  # All should build successfully
  expect_s3_class(p_line, "ggplot")
  expect_s3_class(p_point, "ggplot")
  expect_s3_class(p_step, "ggplot")
  expect_s3_class(p_area, "ggplot")
  expect_s3_class(p_combined, "ggplot")
})

test_that("tf_ggplot edge cases render properly", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  # Test various edge cases
  set.seed(222324)

  # 1. Single function
  data1 <- data.frame(id = 1)
  data1$func <- tf_rgp(1, arg = seq(0, 1, length.out = 21))

  p1 <- tf_ggplot(data1, aes(tf = func)) +
    suppressWarnings(geom_line(color = "blue", linewidth = 1)) +
    labs(title = "Edge Case: Single Function") +
    theme_minimal()

  # 2. Very sparse data (few evaluation points)
  data2 <- data.frame(id = 1:3)
  data2$func <- tf_rgp(3, arg = c(0, 0.5, 1)) # Only 3 points

  p2 <- tf_ggplot(data2, aes(tf = func, color = factor(id))) +
    suppressWarnings(geom_line(linewidth = 1)) +
    suppressWarnings(geom_point(linewidth = 3)) +
    labs(title = "Edge Case: Very Sparse Data (3 points)", color = "Function") +
    theme_minimal()

  # 3. Many functions (visual clutter test)
  data3 <- data.frame(id = 1:20)
  data3$group <- factor(rep(c("A", "B"), each = 10))
  data3$func <- tf_rgp(20, arg = seq(0, 1, length.out = 51))

  p3 <- tf_ggplot(data3, aes(tf = func, color = group)) +
    suppressWarnings(geom_line(alpha = 0.3, linewidth = 0.5)) +
    labs(
      title = "Edge Case: Many Functions (20)",
      subtitle = "Should be readable with transparency"
    ) +
    theme_minimal()

  # 4. Wide evaluation range
  wide_arg <- seq(-10, 10, length.out = 101)
  data4 <- data.frame(id = 1:2)
  data4$func <- tfd(
    list(
      exp(-wide_arg^2 / 10), # Gaussian
      sin(wide_arg) * exp(-abs(wide_arg) / 5) # Damped oscillation
    ),
    arg = wide_arg
  )

  p4 <- tf_ggplot(data4, aes(tf = func, color = factor(id))) +
    suppressWarnings(geom_line(linewidth = 1)) +
    labs(
      title = "Edge Case: Wide Evaluation Range (-10 to 10)",
      color = "Function"
    ) +
    theme_minimal()

  save_test_plot(p1, "14_single_function")
  save_test_plot(p2, "15_sparse_data")
  save_test_plot(p3, "16_many_functions")
  save_test_plot(p4, "17_wide_range")

  # All should render without errors
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_s3_class(p3, "ggplot")
  expect_s3_class(p4, "ggplot")
})

# Helper function to run all visual tests at once
run_visual_tests <- function(display = interactive(), save_plots = TRUE) {
  cat("Running visual tests for tf_ggplot...\n")
  cat("Plots will be saved to:", setup_visual_tests(), "\n")

  # Set global options for this test run
  old_display <- getOption("tf_ggplot_test_display", interactive())
  old_save <- getOption("tf_ggplot_test_save", TRUE)

  options(tf_ggplot_test_display = display)
  options(tf_ggplot_test_save = save_plots)

  tryCatch(
    {
      # Run the visual tests
      test_file("test-tf-ggplot-visual.R")
      cat("Visual tests completed successfully!\n")
    },
    error = function(e) {
      cat("Error in visual tests:", e$message, "\n")
    },
    finally = {
      # Restore options
      options(tf_ggplot_test_display = old_display)
      options(tf_ggplot_test_save = old_save)
    }
  )
}

# Print helper message
if (interactive()) {
  cat("Visual testing functions available:\n")
  cat("- run_visual_tests()  # Run all visual tests\n")
  cat("- Test plots saved to: test-plots/\n")
  cat("- Set display=TRUE to show plots interactively\n")
}
