# Tests for tf_ggplot axis labeling

test_that("simple tf aesthetic has meaningful axis labels", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- data.frame(id = 1:2)
  data$curves <- tf_rgp(2, arg = seq(0, 1, length.out = 5))

  p <- tf_ggplot(data, aes(tf = curves)) + geom_line()
  built <- suppressWarnings(ggplot_build(p))

  # For simple column references, y-label should be the column name
  expect_equal(built$plot$labels$y, "curves")
  expect_equal(built$plot$labels$x, "curves.arg")
})

test_that("complex tf expression has meaningful axis labels", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- data.frame(id = 1:2)
  data$func1 <- tf_rgp(2, arg = seq(0, 1, length.out = 5))
  data$func2 <- tf_rgp(2, arg = seq(0, 1, length.out = 5))

  p <- tf_ggplot(data, aes(tf = func1 + func2)) + geom_line()
  built <- suppressWarnings(ggplot_build(p))

  # For complex expressions, y-label should be the expression text
  expect_equal(built$plot$labels$y, "func1 + func2")
  expect_equal(built$plot$labels$x, "func1 + func2.arg")
})

test_that("nested tf expression has meaningful axis labels", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- data.frame(id = 1:2)
  data$curves <- tf_rgp(2, arg = seq(-1, 1, length.out = 5))

  p <- tf_ggplot(data, aes(tf = abs(curves))) + geom_line()
  built <- suppressWarnings(ggplot_build(p))

  # For nested expressions, y-label should be the expression text
  expect_equal(built$plot$labels$y, "abs(curves)")
  expect_equal(built$plot$labels$x, "abs(curves).arg")
})

test_that("multiple tf expressions have distinct meaningful labels", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- data.frame(id = 1:2)
  data$func1 <- tf_rgp(2, arg = seq(0, 1, length.out = 5))
  data$func2 <- tf_rgp(2, arg = seq(0, 1, length.out = 5))

  p <- tf_ggplot(data, aes(tf = func1 + func2)) +
    geom_line() +
    suppressWarnings(geom_line(aes(tf = func1), color = "red")) +
    suppressWarnings(geom_line(aes(tf = func2), color = "blue"))

  built <- suppressWarnings(ggplot_build(p))

  # Base plot should have the sum expression label
  expect_equal(built$plot$labels$y, "func1 + func2")

  # The transformed data should have columns for each expression with meaningful names
  data_cols <- names(built$data[[1]])
  # Should not have ugly .tf_expr_ names in the actual plot data
  expect_false(any(grepl("^\\.tf_expr_", data_cols)))
})
