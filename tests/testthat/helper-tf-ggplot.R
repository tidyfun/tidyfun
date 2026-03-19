# Helper functions for tf_ggplot testing

# Create simple test tf data
create_test_tf_data <- function(n_funcs = 3, n_points = 11, seed = 123) {
  set.seed(seed)
  data <- data.frame(
    id = seq_len(n_funcs),
    group = factor(sample(c("A", "B"), n_funcs, replace = TRUE))
  )
  data$func <- tf_rgp(n_funcs, arg = seq(0, 1, length.out = n_points))
  data
}

# Create test data with multiple tf columns
create_multi_tf_data <- function(n_funcs = 2, n_points = 11, seed = 123) {
  set.seed(seed)
  data <- data.frame(id = seq_len(n_funcs))
  data$func1 <- tf_rgp(n_funcs, arg = seq(0, 1, length.out = n_points))
  data$func2 <- tf_rgp(n_funcs, arg = seq(0, 1, length.out = n_points))
  data$func3 <- tf_rgp(n_funcs, arg = seq(0, 1, length.out = n_points))
  data
}

# Create test data with confidence bands
create_band_tf_data <- function(n_funcs = 2, n_points = 11, seed = 123) {
  set.seed(seed)
  data <- data.frame(id = seq_len(n_funcs))

  # Create mean function
  data$mean_func <- tf_rgp(n_funcs, arg = seq(0, 1, length.out = n_points))

  # Create confidence bands by adding/subtracting noise
  mean_vals <- tf_evaluations(data$mean_func)
  lower_vals <- map(mean_vals, ~ .x - abs(rnorm(length(.x), 0, 0.1)))
  upper_vals <- map(mean_vals, ~ .x + abs(rnorm(length(.x), 0, 0.1)))

  data$lower_func <- tfd(lower_vals, arg = seq(0, 1, length.out = n_points))
  data$upper_func <- tfd(upper_vals, arg = seq(0, 1, length.out = n_points))

  data
}

# Check if a ggplot object has expected aesthetic mappings
check_plot_mappings <- function(plot, expected_aes) {
  built <- ggplot_build(plot)
  actual_aes <- names(built$data[[1]])
  all(expected_aes %in% actual_aes)
}

# Count unique groups in plot data
count_plot_groups <- function(plot) {
  built <- ggplot_build(plot)
  length(unique(built$data[[1]]$group))
}

# Extract x values from plot data
get_plot_x_values <- function(plot) {
  built <- ggplot_build(plot)
  sort(unique(built$data[[1]]$x))
}

# Check if plot data has expected number of rows
check_plot_nrows <- function(plot, expected_rows) {
  built <- ggplot_build(plot)
  nrow(built$data[[1]]) == expected_rows
}

# Test if tf_ggplot functionality is available
skip_if_no_tf_ggplot <- function() {
  skip_if_not(exists("tf_ggplot"), "tf_ggplot function not available")
}

capture_warnings_silently <- function(expr) {
  warnings <- character(0)
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  list(value = value, warnings = warnings)
}
