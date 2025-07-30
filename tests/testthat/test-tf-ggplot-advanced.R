# Advanced tf_ggplot functionality tests
# These test more complex scenarios and edge cases

test_that("multiple tf aesthetics work correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_multi_tf_data(n_funcs = 2, n_points = 5)

  # Test tf_x and tf_y aesthetics
  p <- tf_ggplot(data) + geom_point(aes(tf_x = func1, tf_y = func2))

  built <- ggplot_build(p)
  plot_data <- built$data[[1]]

  # Should have both x and y from tf objects
  expect_true(all(c("x", "y") %in% names(plot_data)))

  # Should have correct number of points (2 functions × 5 evaluations)
  expect_equal(nrow(plot_data), 10)

  # Should have 2 groups (one per function pair)
  expect_equal(length(unique(plot_data$group)), 2)
})

test_that("ribbon geoms work with tf_ymin and tf_ymax", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_band_tf_data(n_funcs = 2, n_points = 5)

  # Test ribbon with tf confidence bands
  p <- tf_ggplot(data) +
    geom_ribbon(aes(tf_ymin = lower_func, tf_ymax = upper_func), alpha = 0.3)

  built <- ggplot_build(p)
  plot_data <- built$data[[1]]

  # Should have ymin and ymax columns
  expect_true(all(c("ymin", "ymax") %in% names(plot_data)))

  # Should have correct structure
  expect_equal(nrow(plot_data), 10) # 2 functions × 5 points
  expect_equal(length(unique(plot_data$group)), 2)

  # ymax should be >= ymin for all points
  expect_true(all(plot_data$ymax >= plot_data$ymin))
})

test_that("combining tf and regular geoms works", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 3, n_points = 5)
  data$mean_val <- c(1, 2, 3)

  # This should work - different aesthetics
  p <- tf_ggplot(data) +
    geom_line(aes(tf = func, color = group)) +
    geom_point(aes(x = 0.5, y = mean_val), size = 3) # Regular geom

  built <- ggplot_build(p)

  # Should have 2 layers
  expect_equal(length(built$data), 2)

  # First layer should be tf data (15 rows: 3 functions × 5 points)
  expect_equal(nrow(built$data[[1]]), 15)

  # Second layer should be regular data (3 points)
  expect_equal(nrow(built$data[[2]]), 3)
})

test_that("faceting works with tf data", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 6, n_points = 5)
  data$treatment <- factor(rep(c("A", "B"), each = 3))

  # Test faceting by treatment
  p <- tf_ggplot(data, aes(tf = func)) +
    geom_line() +
    facet_wrap(~treatment)

  built <- ggplot_build(p)

  # Should have 2 panels (one per treatment)
  expect_equal(length(unique(built$layout$layout$PANEL)), 2)

  # Faceting variable should be preserved in data
  plot_data <- built$data[[1]]
  expect_true("treatment" %in% names(plot_data))
  expect_equal(length(unique(plot_data$treatment)), 2)
})

test_that("irregular tf objects are handled correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  # Create irregular tf data by sparsifying
  data <- data.frame(id = 1:2)
  regular_tf <- tf_rgp(2, arg = seq(0, 1, length.out = 11))
  data$irreg_func <- tf_sparsify(regular_tf, prob = 0.5)

  # Should still work with irregular data
  p <- tf_ggplot(data, aes(tf = irreg_func)) + geom_line()

  built <- ggplot_build(p)
  plot_data <- built$data[[1]]

  # Should have data for both functions
  expect_equal(length(unique(plot_data$group)), 2)
  expect_true(nrow(plot_data) > 0)

  # Should have fewer points than regular data due to sparsification
  expect_true(nrow(plot_data) <= 2 * 11)
})

test_that("different geom types work with tf aesthetics", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 2, n_points = 5)

  # Test different geoms
  p_line <- tf_ggplot(data, aes(tf = func)) + geom_line()
  p_point <- tf_ggplot(data, aes(tf = func)) + geom_point()
  p_step <- tf_ggplot(data, aes(tf = func)) + geom_step()
  p_area <- tf_ggplot(data, aes(tf = func)) + geom_area()

  # All should build successfully
  expect_s3_class(ggplot_build(p_line), "ggplot_built")
  expect_s3_class(ggplot_build(p_point), "ggplot_built")
  expect_s3_class(ggplot_build(p_step), "ggplot_built")
  expect_s3_class(ggplot_build(p_area), "ggplot_built")

  # All should have same basic structure
  expect_equal(count_plot_groups(p_line), 2)
  expect_equal(count_plot_groups(p_point), 2)
  expect_equal(count_plot_groups(p_step), 2)
  expect_equal(count_plot_groups(p_area), 2)
})

test_that("complex aesthetic mappings work", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 4, n_points = 5)
  data$treatment <- factor(rep(c("A", "B"), each = 2))
  data$subject <- factor(1:4)
  data$baseline <- rnorm(4)

  # Complex mapping with multiple aesthetics
  p <- tf_ggplot(
    data,
    aes(tf = func, color = treatment, linetype = subject, alpha = baseline)
  ) +
    geom_line()

  built <- ggplot_build(p)
  plot_data <- built$data[[1]]

  # Should preserve all aesthetic mappings
  expected_aes <- c("colour", "linetype", "alpha")
  expect_true(all(expected_aes %in% names(plot_data)))

  # Should have correct grouping
  expect_equal(length(unique(plot_data$group)), 4)

  # Aesthetics should be consistent within groups
  aesthetics_by_group <- split(plot_data[expected_aes], plot_data$group)
  consistent <- all(sapply(aesthetics_by_group, function(x) {
    sapply(x, function(col) length(unique(col)) == 1)
  }))
  expect_true(consistent)
})

test_that("performance warnings are triggered appropriately", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  # Create potentially large dataset
  data <- data.frame(id = 1:50) # Many functions
  data$func <- tf_rgp(50, arg = seq(0, 1, length.out = 51)) # Dense grid

  # Should warn about large data expansion
  expect_warning(
    suppress_tf_warnings({
      tf_ggplot(data, aes(tf = func)) + geom_line()
    }),
    "large.*data|expansion|memory|performance"
  )
})

test_that("scale conflicts are detected", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 2, n_points = 5)
  data$scalar_y <- rnorm(2)

  # This creates a scale conflict (tf values and scalar values on same y-scale)
  expect_warning(
    suppress_tf_warnings({
      p <- tf_ggplot(data) +
        geom_line(aes(tf = func)) + # tf data on y-scale
        geom_point(aes(x = 0.5, y = scalar_y)) # scalar data on y-scale
    }),
    "scale.*conflict|mixed.*aesthetic"
  )
})

test_that("empty or NA tf objects are handled gracefully", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  # Create data with some NA functions
  data <- data.frame(id = 1:3)
  data$func <- tf_rgp(3, arg = seq(0, 1, length.out = 5))

  # Set one function to NA
  data$func[2] <- NA

  # Should handle NA functions gracefully
  p <- tf_ggplot(data, aes(tf = func)) + geom_line()

  built <- ggplot_build(p)
  plot_data <- built$data[[1]]

  # Should only have data for non-NA functions
  expect_equal(length(unique(plot_data$group)), 2) # Only 2 non-NA functions
  expect_equal(nrow(plot_data), 2 * 5) # 2 functions × 5 points
})

test_that("theme and scale customization works with tf_ggplot", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 2, n_points = 5)

  # Test that standard ggplot2 customization works
  p <- tf_ggplot(data, aes(tf = func, color = group)) +
    geom_line() +
    scale_color_manual(values = c("A" = "red", "B" = "blue")) +
    theme_minimal() +
    labs(title = "Test Plot", x = "Time", y = "Value")

  # Should build successfully with customizations
  built <- ggplot_build(p)
  expect_s3_class(built, "ggplot_built")

  # Check that labels are preserved
  expect_equal(p$labels$title, "Test Plot")
  expect_equal(p$labels$x, "Time")
  expect_equal(p$labels$y, "Value")
})
