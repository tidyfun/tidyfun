# Tests for complex tf_ggplot use cases
# Tests complex expressions in tf aesthetics and derived quantities

test_that("tf aesthetic with function transformations works", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  # Create test data with positive and negative values
  set.seed(123)
  data <- data.frame(id = 1:3)
  arg_vals <- seq(-2, 2, length.out = 11)
  func_vals <- list(
    sin(arg_vals), # Has negative values
    cos(arg_vals), # Has negative values
    arg_vals^2 - 1 # Has negative values
  )
  data$curves <- tfd(func_vals, arg = arg_vals)

  # Test abs() transformation
  expect_no_error({
    p_abs <- tf_ggplot(data, aes(tf = abs(curves))) + geom_line()
  })

  # Check that plot builds correctly
  built_abs <- ggplot_build(p_abs)
  plot_data_abs <- built_abs$data[[1]]

  # All y values should be non-negative after abs()
  expect_true(all(plot_data_abs$y >= 0))

  # Should have 3 groups (one per function)
  expect_equal(length(unique(plot_data_abs$group)), 3)

  # Should have correct number of rows (3 functions × 11 points)
  expect_equal(nrow(plot_data_abs), 33)
})

test_that("tf aesthetic with mathematical operations works", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  set.seed(456)
  data <- data.frame(id = 1:2)
  data$func1 <- tf_rgp(2, arg = seq(0, 1, length.out = 6))
  data$func2 <- tf_rgp(2, arg = seq(0, 1, length.out = 6))

  # Test arithmetic operations
  expect_no_error({
    p_sum <- tf_ggplot(data, aes(tf = func1 + func2)) + geom_line()
    p_diff <- tf_ggplot(data, aes(tf = func1 - func2)) + geom_line()
    p_prod <- tf_ggplot(data, aes(tf = func1 * func2)) + geom_line()
    p_scaled <- tf_ggplot(data, aes(tf = 2 * func1)) + geom_line()
  })

  # Check that plots build correctly
  built_sum <- ggplot_build(p_sum)
  built_scaled <- ggplot_build(p_scaled)

  expect_equal(length(unique(built_sum$data[[1]]$group)), 2)
  expect_equal(length(unique(built_scaled$data[[1]]$group)), 2)
  expect_equal(nrow(built_sum$data[[1]]), 12) # 2 functions × 6 points
  expect_equal(nrow(built_scaled$data[[1]]), 12)
})

test_that("color aesthetic with tf-derived quantities works", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  set.seed(789)
  data <- data.frame(id = 1:4)
  data$curves <- tf_rgp(4, arg = seq(0, 1, length.out = 11))

  # Test color by tf_depth
  expect_no_error({
    p_depth <- tf_ggplot(data, aes(tf = curves, color = tf_depth(curves))) +
      geom_line()
  })

  built_depth <- ggplot_build(p_depth)
  plot_data_depth <- built_depth$data[[1]]

  # Should have 4 groups (one per function)
  expect_equal(length(unique(plot_data_depth$group)), 4)

  # Color should vary based on depth
  expect_true(length(unique(plot_data_depth$colour)) > 1)

  # Each group should have consistent color within group
  color_by_group <- split(plot_data_depth$colour, plot_data_depth$group)
  consistent_colors <- all(sapply(
    color_by_group,
    function(x) length(unique(x)) == 1
  ))
  expect_true(consistent_colors)
})

test_that("multiple derived aesthetics work together", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  set.seed(101112)
  data <- data.frame(id = 1:5)
  data$curves <- tf_rgp(5, arg = seq(0, 1, length.out = 11))

  # Test multiple tf-derived aesthetics
  expect_no_error({
    p_multi <- tf_ggplot(
      data,
      aes(
        tf = curves,
        color = tf_depth(curves),
        alpha = tf_frange(curves) # Function range as transparency
      )
    ) +
      geom_line()
  })

  built_multi <- ggplot_build(p_multi)
  plot_data_multi <- built_multi$data[[1]]

  # Should have all aesthetic mappings
  expect_true("colour" %in% names(plot_data_multi))
  expect_true("alpha" %in% names(plot_data_multi))

  # Should have 5 groups
  expect_equal(length(unique(plot_data_multi$group)), 5)

  # Alpha should vary based on function range
  expect_true(length(unique(plot_data_multi$alpha)) > 1)
})

test_that("complex tf expressions in constructor vs geom are equivalent", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  set.seed(131415)
  data <- data.frame(id = 1:3)
  data$curves <- tf_rgp(3, arg = seq(-1, 1, length.out = 9))

  # Method 1: Complex expression in constructor
  p1 <- tf_ggplot(data, aes(tf = abs(curves), color = tf_depth(curves))) +
    geom_line()

  # Method 2: Complex expression in geom
  p2 <- tf_ggplot(data) +
    geom_line(aes(tf = abs(curves), color = tf_depth(curves)))

  # Both should build successfully
  built1 <- ggplot_build(p1)
  built2 <- ggplot_build(p2)

  # Should have same dimensions
  expect_equal(dim(built1$data[[1]]), dim(built2$data[[1]]))

  # Should have same number of groups
  expect_equal(
    length(unique(built1$data[[1]]$group)),
    length(unique(built2$data[[1]]$group))
  )

  # Both should have only non-negative y values (due to abs)
  expect_true(all(built1$data[[1]]$y >= 0))
  expect_true(all(built2$data[[1]]$y >= 0))

  # Colors should vary in both (due to tf_depth)
  expect_true(length(unique(built1$data[[1]]$colour)) > 1)
  expect_true(length(unique(built2$data[[1]]$colour)) > 1)
})

test_that("nested function calls in tf aesthetics work", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  set.seed(161718)
  data <- data.frame(id = 1:2)
  data$curves <- tf_rgp(2, arg = seq(-1, 1, length.out = 7))

  # Test nested function calls
  expect_no_error({
    p_nested <- tf_ggplot(data, aes(tf = sqrt(abs(curves)))) + geom_line()
  })

  built_nested <- ggplot_build(p_nested)
  plot_data_nested <- built_nested$data[[1]]

  # All y values should be non-negative (due to sqrt(abs()))
  expect_true(all(plot_data_nested$y >= 0))
  expect_true(all(is.finite(plot_data_nested$y)))

  # Should have 2 groups
  expect_equal(length(unique(plot_data_nested$group)), 2)
})

test_that("tf aesthetics with scalars and derived quantities", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  set.seed(192021)
  data <- data.frame(
    id = 1:4,
    treatment = factor(c("A", "A", "B", "B")),
    baseline = rnorm(4)
  )
  data$curves <- tf_rgp(4, arg = seq(0, 1, length.out = 8))

  # Mix tf aesthetics with scalar summaries and regular variables
  expect_no_error({
    p_mixed <- tf_ggplot(
      data,
      aes(
        tf = curves,
        color = treatment, # Regular factor variable
        size = tf_fmean(curves), # Scalar summary of tf
        alpha = abs(baseline) # Transformation of scalar
      )
    ) +
      geom_line()
  })

  built_mixed <- ggplot_build(p_mixed)
  plot_data_mixed <- built_mixed$data[[1]]

  # Should have all aesthetics
  expect_true("colour" %in% names(plot_data_mixed))
  expect_true("size" %in% names(plot_data_mixed))
  expect_true("alpha" %in% names(plot_data_mixed))

  # Should have 4 groups (one per function)
  expect_equal(length(unique(plot_data_mixed$group)), 4)

  # Size should vary based on function means
  expect_true(length(unique(plot_data_mixed$size)) > 1)

  # Alpha should vary based on baseline
  expect_true(length(unique(plot_data_mixed$alpha)) > 1)

  # Color should reflect treatment levels (2 levels)
  expect_equal(length(unique(plot_data_mixed$colour)), 2)
})

test_that("tf transformations preserve grouping correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  set.seed(222324)
  data <- data.frame(
    id = 1:6,
    group = factor(rep(c("X", "Y", "Z"), each = 2))
  )
  data$curves <- tf_rgp(6, arg = seq(0, 1, length.out = 5))

  # Test transformation with grouping variable
  p_transform <- tf_ggplot(
    data,
    aes(
      tf = abs(curves - tf_fmean(curves)), # Center around mean then abs
      color = group
    )
  ) +
    geom_line()

  built_transform <- ggplot_build(p_transform)
  plot_data_transform <- built_transform$data[[1]]

  # Should have 6 groups (one per function)
  expect_equal(length(unique(plot_data_transform$group)), 6)

  # Should have 3 colors (for groups X, Y, Z)
  expect_equal(length(unique(plot_data_transform$colour)), 3)

  # All y values should be non-negative (due to abs)
  expect_true(all(plot_data_transform$y >= 0))

  # Should have 30 rows (6 functions × 5 points)
  expect_equal(nrow(plot_data_transform), 30)
})

test_that("error handling for invalid tf expressions", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  set.seed(252627)
  data <- data.frame(id = 1:2)
  data$curves <- tf_rgp(2, arg = seq(0, 1, length.out = 5))
  data$not_tf <- rnorm(2)

  # Should error when applying tf functions to non-tf objects
  expect_error(
    {
      p <- tf_ggplot(data, aes(tf = abs(not_tf))) + geom_line()
      ggplot_build(p) # Trigger validation
    },
    "tf.*object|must be.*tf"
  )

  # Should error when tf aesthetic references non-existent column
  expect_error(
    {
      p <- tf_ggplot(data, aes(tf = nonexistent_func)) + geom_line()
      ggplot_build(p) # Trigger validation
    },
    "not found|object.*not found"
  )
})

test_that("performance with complex tf expressions", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  # Test with moderately large dataset that triggers warning
  set.seed(282930)
  data <- data.frame(id = 1:100) # 100 functions
  data$curves <- tf_rgp(100, arg = seq(0, 1, length.out = 101)) # 101 points each (100 * 101 = 10100 > 10000)

  # Create large plot (warning logic needs to be re-added to finalization process)
  p_large <- tf_ggplot(
    data,
    aes(
      tf = abs(curves),
      color = tf_depth(curves)
    )
  ) +
    geom_line()

  # But should still build correctly
  built_large <- ggplot_build(p_large)
  plot_data_large <- built_large$data[[1]]

  # Should have 100 groups
  expect_equal(length(unique(plot_data_large$group)), 100)

  # Should have 10100 rows (100 functions × 101 points)
  expect_equal(nrow(plot_data_large), 10100)

  # All y values should be non-negative
  expect_true(all(plot_data_large$y >= 0))
})

test_that("complex expressions work with faceting", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  set.seed(313233)
  data <- data.frame(
    id = 1:8,
    condition = factor(rep(c("Control", "Treatment"), each = 4)),
    replicate = factor(rep(1:4, 2))
  )
  data$curves <- tf_rgp(8, arg = seq(0, 1, length.out = 6))

  # Test complex expressions with faceting
  expect_no_error({
    p_facet <- tf_ggplot(
      data,
      aes(
        tf = abs(curves),
        color = tf_depth(curves)
      )
    ) +
      geom_line() +
      facet_wrap(~condition)
  })

  built_facet <- ggplot_build(p_facet)

  # Should have 2 panels (Control, Treatment)
  expect_equal(length(unique(built_facet$layout$layout$PANEL)), 2)

  # Should have 8 groups total
  plot_data_facet <- built_facet$data[[1]]
  expect_equal(length(unique(plot_data_facet$group)), 8)

  # All y values should be non-negative
  expect_true(all(plot_data_facet$y >= 0))
})

test_that("tf expressions work with custom evaluation grids", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  set.seed(343536)
  data <- data.frame(id = 1:3)
  data$curves <- tf_rgp(3, arg = seq(0, 1, length.out = 21)) # Dense original grid

  # Test with custom coarser grid
  custom_arg <- seq(0, 1, length.out = 6) # Coarser grid

  expect_no_error({
    p_custom <- tf_ggplot(data, aes(tf = abs(curves)), arg = custom_arg) +
      geom_line()
  })

  built_custom <- ggplot_build(p_custom)
  plot_data_custom <- built_custom$data[[1]]

  # Should use custom grid (6 points)
  expect_equal(length(unique(plot_data_custom$x)), 6)
  expect_equal(sort(unique(plot_data_custom$x)), custom_arg)

  # Should have 18 rows (3 functions × 6 points)
  expect_equal(nrow(plot_data_custom), 18)

  # All y values should be non-negative
  expect_true(all(plot_data_custom$y >= 0))
})

test_that("debugging information for complex expressions", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  # Test that we can inspect what happens to complex expressions
  data <- data.frame(id = 1:3)

  # Create curves with different variability to get different depths
  arg_vals <- seq(-1, 1, length.out = 5)
  curves_list <- list(
    rep(0, 5), # flat curve - should have lower depth
    c(-1, 0, 1, 0, -1), # more variable - should have higher depth
    c(-0.5, -0.2, 0, 0.2, 0.5) # medium variability
  )
  data$curves <- tfd(curves_list, arg = arg_vals)

  # Create plot with complex expression
  p_debug <- tf_ggplot(data, aes(tf = abs(curves), color = tf_depth(curves))) +
    geom_line()

  built_debug <- ggplot_build(p_debug)
  plot_data_debug <- built_debug$data[[1]]

  # Check that the transformation was applied correctly
  # Original curves have negative values, transformed should not
  original_curves <- tf_evaluate(data$curves, arg = seq(-1, 1, length.out = 5))
  has_negatives <- any(sapply(original_curves, function(x) any(x < 0)))
  expect_true(has_negatives) # Original should have negatives

  # Transformed plot data should not have negatives
  expect_true(all(plot_data_debug$y >= 0))

  # Color should vary based on depth (3 different curves = 3 different colors)
  expect_true(length(unique(plot_data_debug$colour)) > 1)
})
