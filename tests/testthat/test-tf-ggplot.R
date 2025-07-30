test_that("tf_ggplot constructor creates proper object", {
  skip_if_not_installed("ggplot2")

  # Create test data
  set.seed(123)
  data <- data.frame(
    id = 1:3,
    group = factor(c("A", "A", "B"))
  )
  data$func <- tf_rgp(3, arg = seq(0, 1, length.out = 11))

  # Test basic constructor
  p <- tf_ggplot(data)
  expect_s3_class(p, "tf_ggplot")
  expect_s3_class(p, "ggplot") # Should inherit from ggplot

  # Test with aesthetic mapping
  p2 <- tf_ggplot(data, aes(tf = func, color = group))
  expect_s3_class(p2, "tf_ggplot")
  expect_equal(length(p2$mapping), 2)

  # Test with arg parameter
  p3 <- tf_ggplot(data, arg = seq(0, 1, length.out = 21))
  expect_s3_class(p3, "tf_ggplot")
  expect_equal(attr(p3, "tf_arg"), seq(0, 1, length.out = 21))
})

test_that("parse_tf_aesthetics correctly identifies tf aesthetics", {
  skip_if_not_installed("ggplot2")

  # Test simple tf aesthetic
  mapping1 <- aes(tf = func, color = group)
  parsed1 <- parse_tf_aesthetics(mapping1)
  expect_named(parsed1, c("tf_aes", "regular_aes"))
  expect_equal(length(parsed1$tf_aes), 1)
  expect_equal(names(parsed1$tf_aes), "tf")
  expect_equal(length(parsed1$regular_aes), 1)

  # Test multiple tf aesthetics
  mapping2 <- aes(tf_x = func1, tf_y = func2, color = group)
  parsed2 <- parse_tf_aesthetics(mapping2)
  expect_equal(length(parsed2$tf_aes), 2)
  expect_equal(names(parsed2$tf_aes), c("tf_x", "tf_y"))

  # Test ribbon aesthetics
  mapping3 <- aes(tf_ymin = lower, tf_ymax = upper, fill = group)
  parsed3 <- parse_tf_aesthetics(mapping3)
  expect_equal(length(parsed3$tf_aes), 2)
  expect_equal(names(parsed3$tf_aes), c("tf_ymin", "tf_ymax"))

  # Test no tf aesthetics
  mapping4 <- aes(x = time, y = value, color = group)
  parsed4 <- parse_tf_aesthetics(mapping4)
  expect_equal(length(parsed4$tf_aes), 0)
  expect_equal(length(parsed4$regular_aes), 3)
})

test_that("transform_tf_data correctly transforms data with single tf aesthetic", {
  skip_if_not_installed("ggplot2")

  # Create test data
  set.seed(123)
  data <- data.frame(
    id = 1:3,
    group = factor(c("A", "A", "B")),
    scalar_val = c(10, 20, 30)
  )
  data$func <- tf_rgp(3, arg = seq(0, 1, length.out = 11))

  # Test transformation
  tf_aesthetics <- list(tf = quo(func))
  result <- transform_tf_data(data, tf_aesthetics)

  # Check dimensions - should have 3 functions × 11 points = 33 rows
  expect_equal(nrow(result), 33)

  # Check required columns exist
  expect_true("func.arg" %in% names(result))
  expect_true("func.value" %in% names(result))
  expect_true("func.id" %in% names(result))

  # Check that non-tf columns are replicated correctly
  expect_true("group" %in% names(result))
  expect_true("scalar_val" %in% names(result))
  expect_equal(length(unique(result$group)), 2) # A and B

  # Check that function IDs are correct
  expect_equal(sort(unique(result$func.id)), 1:3)

  # Check that argument values are correct
  expect_equal(sort(unique(result$func.arg)), seq(0, 1, length.out = 11))
})

test_that("transform_tf_data handles multiple tf aesthetics", {
  skip_if_not_installed("ggplot2")

  # Create test data with two tf columns
  set.seed(123)
  data <- data.frame(id = 1:2)
  data$func1 <- tf_rgp(2, arg = seq(0, 1, length.out = 11))
  data$func2 <- tf_rgp(2, arg = seq(0, 1, length.out = 11))

  # Test transformation with multiple tf aesthetics
  tf_aesthetics <- list(tf_x = quo(func1), tf_y = quo(func2))
  result <- transform_tf_data(data, tf_aesthetics)

  # Check that both tf objects are transformed
  expect_true("func1.arg" %in% names(result))
  expect_true("func1.value" %in% names(result))
  expect_true("func2.arg" %in% names(result))
  expect_true("func2.value" %in% names(result))

  # Should have one row per function pair per evaluation point
  expect_equal(nrow(result), 2 * 11) # 2 functions × 11 points
})

test_that("tf_ggplot + geom_line creates correct plot", {
  skip_if_not_installed("ggplot2")

  # Create test data
  set.seed(123)
  data <- data.frame(
    id = 1:3,
    group = factor(c("A", "A", "B"))
  )
  data$func <- tf_rgp(3, arg = seq(0, 1, length.out = 11))

  # Test basic line plot
  p <- tf_ggplot(data, aes(tf = func))
  p_with_geom <- p + geom_line()

  # Should return a regular ggplot object after transformation
  expect_s3_class(p_with_geom, "ggplot")
  expect_false(inherits(p_with_geom, "tf_ggplot")) # Should be converted

  # Check that the plot has the right data structure
  built <- ggplot_build(p_with_geom)
  plot_data <- built$data[[1]]

  # Should have the right number of groups (one per function)
  expect_equal(length(unique(plot_data$group)), 3)

  # Should have x and y columns (transformed from tf)
  expect_true("x" %in% names(plot_data))
  expect_true("y" %in% names(plot_data))
})

test_that("tf_ggplot handles grouping aesthetics correctly", {
  skip_if_not_installed("ggplot2")

  # Create test data
  set.seed(123)
  data <- data.frame(
    id = 1:4,
    treatment = factor(c("A", "A", "B", "B")),
    subject = factor(1:4)
  )
  data$func <- tf_rgp(4, arg = seq(0, 1, length.out = 11))

  # Test with color grouping
  p <- tf_ggplot(data, aes(tf = func, color = treatment)) + geom_line()
  built <- ggplot_build(p)
  plot_data <- built$data[[1]]

  # Should have 4 groups (one per function) but 2 colors (treatments)
  expect_equal(length(unique(plot_data$group)), 4)
  expect_equal(length(unique(plot_data$colour)), 2)

  # Color should be consistent within each function
  color_by_group <- split(plot_data$colour, plot_data$group)
  expect_true(all(sapply(color_by_group, function(x) length(unique(x)) == 1)))
})

test_that("tf_ggplot + geom_ribbon works with tf_ymin/tf_ymax", {
  skip_if_not_installed("ggplot2")

  # Create test data with confidence bands
  set.seed(123)
  data <- data.frame(id = 1:2)
  data$mean_func <- tf_rgp(2, arg = seq(0, 1, length.out = 11))
  data$lower_func <- tf_rgp(2, arg = seq(0, 1, length.out = 11))
  data$upper_func <- tf_rgp(2, arg = seq(0, 1, length.out = 11))

  # Test ribbon plot
  p <- tf_ggplot(data) +
    geom_ribbon(aes(tf_ymin = lower_func, tf_ymax = upper_func), alpha = 0.3)

  expect_s3_class(p, "ggplot")
  built <- ggplot_build(p)
  plot_data <- built$data[[1]]

  # Should have ymin and ymax columns
  expect_true("ymin" %in% names(plot_data))
  expect_true("ymax" %in% names(plot_data))
})

test_that("tf_ggplot validates input correctly", {
  skip_if_not_installed("ggplot2")

  # Test with non-tf data
  data <- data.frame(
    id = 1:3,
    regular_col = rnorm(3)
  )

  # Should work fine if no tf aesthetics are used
  p1 <- tf_ggplot(data, aes(x = id, y = regular_col))
  expect_s3_class(p1, "tf_ggplot")

  # Should error if tf aesthetic references non-tf column
  expect_error(
    tf_ggplot(data, aes(tf = regular_col)) + geom_line(),
    "must be a tf object"
  )

  # Should error if tf aesthetic references non-existent column
  expect_error(
    tf_ggplot(data, aes(tf = nonexistent_col)) + geom_line(),
    "object.*not found|not found"
  )
})

test_that("tf_ggplot handles irregular tf objects", {
  skip_if_not_installed("ggplot2")

  # Create irregular tf data
  set.seed(123)
  data <- data.frame(id = 1:2)
  # Create irregular tf by jiggling and sparsifying
  regular_tf <- tf_rgp(2, arg = seq(0, 1, length.out = 21))
  data$irreg_func <- tf_sparsify(regular_tf, prob = 0.7) # Make sparse/irregular

  # Should still work
  p <- tf_ggplot(data, aes(tf = irreg_func)) + geom_line()
  expect_s3_class(p, "ggplot")

  built <- ggplot_build(p)
  plot_data <- built$data[[1]]

  # Should have data for both functions
  expect_equal(length(unique(plot_data$group)), 2)
  expect_true(nrow(plot_data) > 0)
})

test_that("tf_ggplot preserves faceting variables", {
  skip_if_not_installed("ggplot2")

  # Create test data with faceting variable
  set.seed(123)
  data <- data.frame(
    id = 1:6,
    treatment = factor(rep(c("A", "B"), each = 3)),
    subject = factor(1:6)
  )
  data$func <- tf_rgp(6, arg = seq(0, 1, length.out = 11))

  # Test with faceting
  p <- tf_ggplot(data, aes(tf = func)) +
    geom_line() +
    facet_wrap(~treatment)

  expect_s3_class(p, "ggplot")
  built <- ggplot_build(p)

  # Should have panels for each treatment level
  expect_equal(length(unique(built$layout$layout$PANEL)), 2)

  # Check that faceting variable is preserved in data
  plot_data <- built$data[[1]]
  expect_true("treatment" %in% names(plot_data))
  expect_equal(length(unique(plot_data$treatment)), 2)
})

test_that("tf_ggplot handles custom arg specification", {
  skip_if_not_installed("ggplot2")

  # Create test data
  set.seed(123)
  data <- data.frame(id = 1:2)
  data$func <- tf_rgp(2, arg = seq(0, 1, length.out = 21))

  # Test with custom arg
  custom_arg <- seq(0, 1, length.out = 11) # Coarser grid
  p <- tf_ggplot(data, aes(tf = func), arg = custom_arg) + geom_line()

  built <- ggplot_build(p)
  plot_data <- built$data[[1]]

  # Should use the custom arg values
  unique_x <- sort(unique(plot_data$x))
  expect_equal(unique_x, custom_arg)
  expect_equal(length(unique_x), 11) # Not the original 21
})

test_that("tf_ggplot error handling for scale conflicts", {
  skip_if_not_installed("ggplot2")

  # Create mixed data
  set.seed(123)
  data <- data.frame(
    id = 1:3,
    scalar_y = rnorm(3)
  )
  data$func <- tf_rgp(3, arg = seq(0, 1, length.out = 11))

  # This should produce a warning about scale conflicts
  # (tf values on y-scale AND scalar values on y-scale)
  expect_warning(
    {
      p <- tf_ggplot(data) +
        geom_line(aes(tf = func)) + # tf data on y-scale
        geom_point(aes(x = id, y = scalar_y)) # scalar data on y-scale
    },
    "scale.*conflict|mixed.*scale"
  )
})

test_that("tf_ggplot performance warnings for large datasets", {
  skip_if_not_installed("ggplot2")

  # Create potentially large dataset
  set.seed(123)
  data <- data.frame(id = 1:100) # Many functions
  data$func <- tf_rgp(100, arg = seq(0, 1, length.out = 101)) # Dense grid

  # Should warn about large expansion
  expect_warning(
    tf_ggplot(data, aes(tf = func)) + geom_line(),
    "large.*expansion|memory|performance"
  )
})
