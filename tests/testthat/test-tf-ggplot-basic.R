# Basic tf_ggplot functionality tests
# These tests focus on core features that must work before more advanced functionality

test_that("tf_ggplot basic constructor works", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 2, n_points = 5)

  # Basic constructor without aesthetics
  p1 <- tf_ggplot(data)
  expect_s3_class(p1, c("tf_ggplot", "ggplot"))
  expect_identical(p1$data, data)

  # Constructor with aesthetics
  p2 <- tf_ggplot(data, aes(tf = func, color = group))
  expect_s3_class(p2, c("tf_ggplot", "ggplot"))
  expect_equal(length(p2$mapping), 2)

  # Constructor with custom arg
  custom_arg <- c(0, 0.5, 1)
  p3 <- tf_ggplot(data, arg = custom_arg)
  expect_equal(attr(p3, "tf_arg"), custom_arg)
})

test_that("basic tf aesthetic transformation works", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 2, n_points = 5)

  # Single tf aesthetic
  p <- tf_ggplot(data, aes(tf = func)) + suppressWarnings(geom_line())

  # Should still be tf_ggplot until finalized
  expect_s3_class(p, "ggplot")
  expect_true(inherits(p, "tf_ggplot"))

  # Check plot builds successfully
  built <- suppressWarnings(ggplot_build(p))
  expect_s3_class(built, "ggplot_built")

  # Should have correct number of groups (one per function)
  expect_equal(count_plot_groups(p), 2)

  # Should have x and y mappings
  expect_true(check_plot_mappings(p, c("x", "y", "group")))
})

test_that("tf aesthetic parsing identifies tf columns correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  # Test different tf aesthetic patterns
  mapping1 <- aes(tf = func)
  parsed1 <- parse_tf_aesthetics(mapping1)
  expect_equal(names(parsed1$tf_aes), "tf")
  expect_equal(length(parsed1$regular_aes), 0)

  mapping2 <- aes(tf = func, color = group)
  parsed2 <- parse_tf_aesthetics(mapping2)
  expect_equal(names(parsed2$tf_aes), "tf")
  expect_equal(length(parsed2$regular_aes), 1)

  mapping3 <- aes(tf_x = func1, tf_y = func2)
  parsed3 <- parse_tf_aesthetics(mapping3)
  expect_equal(sort(names(parsed3$tf_aes)), c("tf_x", "tf_y"))
  expect_equal(length(parsed3$regular_aes), 0)

  # Test no tf aesthetics
  mapping4 <- aes(x = time, y = value)
  parsed4 <- parse_tf_aesthetics(mapping4)
  expect_equal(length(parsed4$tf_aes), 0)
  expect_equal(length(parsed4$regular_aes), 2)
})

test_that("data transformation creates correct structure", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  # Create test data with deterministic groups
  data <- data.frame(
    id = 1:2,
    group = factor(c("A", "B"))
  )
  data$func <- tf_rgp(2, arg = seq(0, 1, length.out = 5))
  tf_aesthetics <- list(tf = quo(func))

  result <- transform_tf_data(data, tf_aesthetics)

  # Check dimensions
  expect_equal(nrow(result), 2 * 5) # 2 functions × 5 points

  # Check required columns
  expected_cols <- c("func.arg", "func.value", "func.id", "group", "id")
  expect_true(all(expected_cols %in% names(result)))

  # Check data integrity
  expect_equal(sort(unique(result$func.id)), c(1, 2))
  expect_equal(length(unique(result$func.arg)), 5)
  expect_equal(length(unique(result$group)), 2) # A and B
})

test_that("geom_line integration produces correct plot structure", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 3, n_points = 6)

  p <- tf_ggplot(data, aes(tf = func, color = group)) +
    suppressWarnings(geom_line())

  # Build plot and check structure
  built <- suppressWarnings(ggplot_build(p))
  plot_data <- built$data[[1]]

  # Should have 3 groups (one per function)
  expect_equal(length(unique(plot_data$group)), 3)

  # Should have 6 unique x values (evaluation points)
  expect_equal(length(unique(plot_data$x)), 6)

  # Should have 18 total rows (3 functions × 6 points)
  expect_equal(nrow(plot_data), 18)

  # Should have color aesthetic preserved
  expect_true("colour" %in% names(plot_data))
})

test_that("error handling for invalid tf aesthetics", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  # Data without tf columns
  data <- data.frame(
    id = 1:3,
    regular_col = rnorm(3)
  )

  # Should error when tf aesthetic references non-tf column
  expect_error(
    suppressWarnings({
      p <- tf_ggplot(data, aes(tf = regular_col)) +
        suppressWarnings(geom_line())
      ggplot_build(p) # Trigger the validation
    }),
    "tf.*object|must be.*tf"
  )

  # Should error when tf aesthetic references non-existent column
  expect_error(
    suppressWarnings({
      p <- tf_ggplot(data, aes(tf = nonexistent)) +
        suppressWarnings(geom_line())
      ggplot_build(p) # Trigger the validation
    }),
    "object.*not found|not found"
  )
})

test_that("custom arg parameter works correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 2, n_points = 11)

  # Use coarser evaluation grid
  custom_arg <- seq(0, 1, length.out = 5)
  p <- tf_ggplot(data, aes(tf = func), arg = custom_arg) +
    suppressWarnings(geom_line())

  # Check that custom arg is used
  x_values <- get_plot_x_values(p)
  expect_equal(x_values, custom_arg)
  expect_equal(length(x_values), 5) # Not original 11

  # Check total rows (2 functions × 5 points)
  expect_true(check_plot_nrows(p, 10))
})

test_that("grouping variables are preserved in transformation", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 4, n_points = 5)
  data$treatment <- factor(c("A", "A", "B", "B"))
  data$subject <- factor(1:4)

  p <- tf_ggplot(data, aes(tf = func, color = treatment)) +
    suppressWarnings(geom_line())
  built <- ggplot_build(p)
  plot_data <- built$data[[1]]

  # Note: unmapped variables are not preserved in final plot data (this is normal ggplot2 behavior)
  # But the color aesthetic should reflect treatment mapping
  expect_true("colour" %in% names(plot_data))

  # Should have correct number of colors (2 treatment levels)
  expect_equal(length(unique(plot_data$colour)), 2)

  # Should have consistent color within each function
  color_by_group <- split(plot_data$colour, plot_data$group)
  consistent_colors <- all(sapply(
    color_by_group,
    function(x) length(unique(x)) == 1
  ))
  expect_true(consistent_colors)
})
