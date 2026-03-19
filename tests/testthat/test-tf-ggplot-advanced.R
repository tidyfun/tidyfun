# Advanced tf_ggplot functionality tests
# These test more complex scenarios and edge cases

test_that("multiple tf aesthetics work correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_multi_tf_data(n_funcs = 2, n_points = 5)

  # Test tf_x and tf_y aesthetics
  p <- tf_ggplot(data) +
    geom_point(aes(tf_x = func1, tf_y = func2))

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
    geom_ribbon(
      aes(tf_ymin = lower_func, tf_ymax = upper_func),
      alpha = 0.3
    )

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

test_that("summary ribbon tf aesthetics broadcast across plot-level tf data", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  set.seed(123)
  data <- data.frame(g = gl(2, 5))
  data$f <- tf_rgp(10) + 5

  expect_no_warning({
    built <- ggplot_build(
      tf_ggplot(data, aes(tf = f, tf_ymin = min(f), tf_ymax = max(f))) +
        geom_ribbon(alpha = 0.01)
    )
  })

  plot_data <- built$data[[1]]
  expect_true(all(c("ymin", "ymax") %in% names(plot_data)))
  expect_true(all(is.finite(plot_data$ymin)))
  expect_true(all(is.finite(plot_data$ymax)))
})

test_that("summary ribbon and summary line layers work without row-size errors", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  set.seed(123)
  data <- data.frame(id = 1:10)
  data$f <- tf_rgp(10) + 5

  expect_no_error({
    built <- ggplot_build(
      tf_ggplot(data) +
        geom_ribbon(
          aes(tf_ymin = mean(f) - sd(f), tf_ymax = mean(f) + sd(f)),
          alpha = 0.3
        ) +
        geom_line(aes(tf = mean(f)))
    )
  })

  expect_equal(length(built$data), 2)
  expect_true(all(c("ymin", "ymax") %in% names(built$data[[1]])))
  expect_true(all(c("x", "y") %in% names(built$data[[2]])))
})

test_that("combining tf and regular geoms works", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 3, n_points = 5)
  data$mean_val <- c(1, 2, 3)

  # This should work - different aesthetics
  captured <- capture_warnings_silently(
    tf_ggplot(data) +
      geom_line(aes(tf = func, color = group)) +
      geom_point(aes(x = 0.5, y = mean_val), size = 3) # Regular geom
  )
  p <- captured$value
  expect_true(any(grepl(
    "Potential.*conflict|scale.*conflict",
    captured$warnings
  )))

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

  # Plot should build successfully and have correct structure
  plot_data <- built$data[[1]]
  expect_equal(length(unique(plot_data$group)), 6) # 6 functions
  expect_equal(nrow(plot_data), 30) # 6 functions × 5 points
})

test_that("irregular tf objects are handled correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  # Create irregular tf data by sparsifying
  data <- data.frame(id = 1:2)
  regular_tf <- tf_rgp(2, arg = seq(0, 1, length.out = 11))
  data$irreg_func <- tf_sparsify(regular_tf, dropout = 0.5)

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

  # Warning triggers only for > 200 functions AND > 100 grid points
  data <- data.frame(id = 1:201)
  data$func <- tf_rgp(201, arg = seq(0, 1, length.out = 101))

  captured <- capture_warnings_silently({
    p <- tf_ggplot(data, aes(tf = func)) + geom_line()
    ggplot_build(p)
  })
  expect_true(any(grepl(
    "Large data expansion",
    captured$warnings
  )))

  # Below threshold: no expansion warning
  data_small <- data.frame(id = 1:100)
  data_small$func <- tf_rgp(100, arg = seq(0, 1, length.out = 101))

  captured_small <- capture_warnings_silently({
    p2 <- tf_ggplot(data_small, aes(tf = func)) + geom_line()
    ggplot_build(p2)
  })
  expect_false(any(grepl(
    "Large data expansion",
    captured_small$warnings
  )))
})

test_that("scale conflicts are detected", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 2, n_points = 5)
  data$scalar_y <- rnorm(2)

  # This creates a scale conflict (tf values and scalar values on same y-scale)
  expect_warning(
    {
      p <- tf_ggplot(data) +
        geom_line(aes(tf = func)) + # tf data on y-scale
        geom_point(aes(x = 0.5, y = scalar_y)) # scalar data on y-scale
    },
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

test_that("multiple tf layers work correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 3, n_points = 5)

  # Create plot with multiple tf layers
  p <- tf_ggplot(data) +
    geom_line(aes(tf = func, color = group), alpha = 0.7) +
    geom_point(aes(tf = func, color = group), size = 2) +
    geom_line(
      aes(tf = func),
      color = "black",
      linetype = "dashed",
      alpha = 0.3
    )

  built <- ggplot_build(p)

  # Should have 3 layers
  expect_equal(length(built$data), 3)

  # All layers should have same number of groups (3 functions)
  expect_equal(length(unique(built$data[[1]]$group)), 3) # colored lines
  expect_equal(length(unique(built$data[[2]]$group)), 3) # colored points
  expect_equal(length(unique(built$data[[3]]$group)), 3) # black dashed lines

  # All layers should have same number of rows (3 functions × 5 points)
  expect_equal(nrow(built$data[[1]]), 15)
  expect_equal(nrow(built$data[[2]]), 15)
  expect_equal(nrow(built$data[[3]]), 15)
})

test_that("mixing multiple tf and regular layers works", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 3, n_points = 5)
  data$mean_val <- c(1, 2, 3)
  data$max_val <- c(1.5, 2.5, 3.5)

  # Create plot mixing tf and regular layers
  captured <- capture_warnings_silently(
    tf_ggplot(data) +
      geom_line(aes(tf = func, color = group)) + # tf layer 1
      geom_point(aes(x = 0.5, y = mean_val, color = group), size = 3) + # regular layer 1
      geom_point(aes(tf = func), alpha = 0.5) + # tf layer 2
      geom_point(aes(x = 0.8, y = max_val, color = group), size = 2) + # regular layer 2
      geom_line(aes(tf = func), linetype = "dotted") # tf layer 3
  )
  expect_true(any(grepl(
    "scale.*conflict|Potential.*conflict",
    captured$warnings
  )))
  p <- captured$value

  built <- ggplot_build(p)

  # Should have 5 layers
  expect_equal(length(built$data), 5)

  # tf layers should have expanded data (15 rows each)
  expect_equal(nrow(built$data[[1]]), 15) # tf lines
  expect_equal(nrow(built$data[[3]]), 15) # tf points
  expect_equal(nrow(built$data[[5]]), 15) # tf dotted lines

  # Regular layers should have original data (3 rows each)
  expect_equal(nrow(built$data[[2]]), 3) # regular points 1
  expect_equal(nrow(built$data[[4]]), 3) # regular points 2
})

test_that("complex multi-layer plots with different aesthetics work", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  # Create more complex data
  data <- create_band_tf_data(n_funcs = 2, n_points = 5)
  data$summary_stat <- c(0.5, -0.5)

  # Create complex multi-layer plot
  expect_warning(
    {
      p <- tf_ggplot(data, aes(color = factor(id))) +
        geom_ribbon(
          aes(tf_ymin = lower_func, tf_ymax = upper_func, fill = factor(id)),
          alpha = 0.2
        ) + # tf ribbon
        geom_line(aes(tf = mean_func), linewidth = 1) + # tf line
        geom_hline(
          aes(yintercept = summary_stat, color = factor(id)),
          linetype = "dashed"
        ) + # regular hlines
        geom_point(aes(tf = mean_func), size = 2) + # tf points
        geom_point(
          aes(x = 0.5, y = summary_stat, color = factor(id)),
          size = 4,
          shape = "diamond"
        ) # regular points
    },
    "scale.*conflict|Potential.*conflict"
  )

  built <- ggplot_build(p)

  # Should have 5 layers
  expect_equal(length(built$data), 5)

  # tf layers should have expanded data (10 rows each: 2 functions × 5 points)
  expect_equal(nrow(built$data[[1]]), 10) # ribbon
  expect_equal(nrow(built$data[[2]]), 10) # line
  expect_equal(nrow(built$data[[4]]), 10) # points

  # Regular layers should have original data (2 rows each)
  expect_equal(nrow(built$data[[3]]), 2) # hlines
  expect_equal(nrow(built$data[[5]]), 2) # points
})

test_that("many layers with different tf expressions work", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 2, n_points = 5)

  # Create plot with many different tf expressions
  p <- tf_ggplot(data, aes(color = group)) +
    geom_line(aes(tf = func), alpha = 0.8) + # original functions
    geom_line(aes(tf = func + 0.5), linetype = "dashed") + # shifted up
    geom_line(aes(tf = func - 0.5), linetype = "dotted") + # shifted down
    geom_point(aes(tf = func, size = tf_fmean(func))) + # points with size by mean
    geom_point(aes(tf = func + 1, alpha = tf_depth(func))) # shifted points with alpha by depth

  built <- ggplot_build(p)

  # Should have 5 layers
  expect_equal(length(built$data), 5)

  # All layers should have same number of rows (2 functions × 5 points = 10)
  for (i in 1:5) {
    expect_equal(
      nrow(built$data[[i]]),
      10,
      info = sprintf("Layer %d should have 10 rows", i)
    )
    expect_equal(
      length(unique(built$data[[i]]$group)),
      2,
      info = sprintf("Layer %d should have 2 groups", i)
    )
  }

  # Check that transformations worked correctly
  # Layer 2 should be shifted up by 0.5 compared to layer 1
  layer1_y <- built$data[[1]]$y[order(built$data[[1]]$x, built$data[[1]]$group)]
  layer2_y <- built$data[[2]]$y[order(built$data[[2]]$x, built$data[[2]]$group)]
  expect_true(
    all(abs((layer2_y - layer1_y) - 0.5) < 1e-10),
    "Layer 2 should be shifted up by 0.5"
  )

  # Layer 3 should be shifted down by 0.5 compared to layer 1
  layer3_y <- built$data[[3]]$y[order(built$data[[3]]$x, built$data[[3]]$group)]
  expect_true(
    all(abs((layer3_y - layer1_y) + 0.5) < 1e-10),
    "Layer 3 should be shifted down by 0.5"
  )
})
