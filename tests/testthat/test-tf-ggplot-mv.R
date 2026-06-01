# Tests for multivariate (tf_mv) support in tf_ggplot

library(ggplot2)

# 2D planar paths -------------------------------------------------------------

test_that("aes(tf = mv) with d == 2 draws a planar trajectory", {
  set.seed(1)
  d <- create_test_tf_mv_data(d = 2, n_funcs = 3, n_points = 11)
  p <- tf_ggplot(d, aes(tf = mv)) + geom_path()
  b <- ggplot_build(p)$data[[1]]

  expect_true(all(c("x", "y", "group") %in% names(b)))
  expect_equal(length(unique(b$group)), 3)
  # x and y are function VALUES (not the [0, 1] arg grid)
  expect_gt(diff(range(b$x)), 1)
  # x comes from component 1, y from component 2
  expect_setequal(
    round(b$x, 8),
    round(as.numeric(unlist(tf_evaluations(tf_component(d$mv, 1)))), 8)
  )
})

test_that("planar path rows are arg-ordered, not x-sorted", {
  set.seed(11)
  d <- create_test_tf_mv_data(d = 2, n_funcs = 1, n_points = 21)
  p <- tf_ggplot(d, aes(tf = mv)) + geom_path()
  b <- ggplot_build(p)$data[[1]]
  # a non-monotone trajectory: x is not sorted (a geom_line would wrongly sort it)
  expect_true(is.unsorted(b$x))
})

test_that("aes(tf_x, tf_y) on two univariate columns draws a planar path", {
  set.seed(2)
  d <- tibble::tibble(fx = tf_rgp(3, 11L), fy = tf_rgp(3, 11L))
  p <- tf_ggplot(d, aes(tf_x = fx, tf_y = fy)) + geom_path()
  b <- ggplot_build(p)$data[[1]]

  expect_equal(length(unique(b$group)), 3)
  # x is fx's values, NOT fy's arg grid (the bug this fix addresses)
  expect_setequal(
    round(b$x, 8),
    round(as.numeric(unlist(tf_evaluations(d$fx))), 8)
  )
  expect_setequal(
    round(b$y, 8),
    round(as.numeric(unlist(tf_evaluations(d$fy))), 8)
  )
})

test_that("tf = mv and tf_x/tf_y on its components are equivalent", {
  set.seed(3)
  d <- create_test_tf_mv_data(d = 2, n_funcs = 4, n_points = 11)
  b1 <- ggplot_build(
    tf_ggplot(d, aes(tf = mv)) + geom_path()
  )$data[[1]]
  b2 <- ggplot_build(
    tf_ggplot(
      d,
      aes(tf_x = tf_component(mv, 1), tf_y = tf_component(mv, 2))
    ) +
      geom_path()
  )$data[[1]]
  expect_equal(b1$x, b2$x)
  expect_equal(b1$y, b2$y)
  expect_equal(as.integer(b1$group), as.integer(b2$group))
})

test_that("tf_mv summaries with length 1 align like univariate summaries", {
  set.seed(32)
  d <- create_test_tf_mv_data(d = 2, n_funcs = 4, n_points = 11)
  b <- ggplot_build(tf_ggplot(d, aes(tf = mean(mv))) + geom_path())$data[[1]]

  expect_equal(length(unique(b$group)), 1)
  expect_equal(nrow(b), 11)
  expect_equal(
    b$x,
    as.numeric(unlist(tf_evaluations(tf_component(mean(d$mv), 1))))
  )
})

# Single-tf-y regression ------------------------------------------------------

test_that("univariate tf still maps x to the arg grid", {
  set.seed(31)
  d <- create_test_tf_data(n_funcs = 3, n_points = 11)
  b <- ggplot_build(tf_ggplot(d, aes(tf = func)) + geom_line())$data[[1]]
  expect_equal(range(b$x), c(0, 1))
  expect_false(is.unsorted(b$x[b$group == b$group[1]]))
})

# Multi-component value-vs-arg ------------------------------------------------

test_that("aes(tf = mv) with d != 2 unnests to long with .component", {
  set.seed(4)
  d <- create_test_tf_mv_data(d = 3, n_funcs = 2, n_points = 11)
  p <- tf_ggplot(d, aes(tf = mv, colour = .component)) + geom_line()
  b <- ggplot_build(p)$data[[1]]

  expect_equal(length(unique(b$group)), 2 * 3) # one group per (curve, component)
  expect_equal(nrow(b), 2 * 3 * 11)
  expect_equal(length(unique(b$colour)), 3)
})

test_that("multi-component plot supports facet_wrap(~ .component)", {
  set.seed(41)
  d <- create_test_tf_mv_data(d = 3, n_funcs = 2, n_points = 11)
  p <- tf_ggplot(d, aes(tf = mv)) + geom_line() + facet_wrap(~.component)
  built <- ggplot_build(p)
  expect_s3_class(built, "ggplot_built")
  expect_equal(length(unique(built$data[[1]]$PANEL)), 3)
})

test_that("type = 'facet' forces value-vs-arg display for d == 2", {
  set.seed(42)
  d <- create_test_tf_mv_data(d = 2, n_funcs = 2, n_points = 11)
  b <- ggplot_build(
    tf_ggplot(d, aes(tf = mv), type = "facet") + geom_line()
  )$data[[1]]
  expect_equal(length(unique(b$group)), 2 * 2)
})

# Irregular / NA handling -----------------------------------------------------

test_that("trajectory on misaligned grids keeps NA so the path breaks", {
  set.seed(5)
  cx <- tfd(matrix(rnorm(2 * 6), 2), arg = seq(0, 0.6, length.out = 6))
  cy <- tfd(matrix(rnorm(2 * 6), 2), arg = seq(0.4, 1.0, length.out = 6))
  mv <- tfd_mv(list(x = cx, y = cy))
  tj <- tidyfun:::.tf_mv_trajectory_long(mv)
  # union grid introduces NAs outside each component's observed range
  expect_true(any(is.na(tj$.mv_x)))
  expect_true(any(is.na(tj$.mv_y)))
  d <- tibble::tibble(mv = mv)
  expect_s3_class(
    suppressWarnings(ggplot_build(tf_ggplot(d, aes(tf = mv)) + geom_path())),
    "ggplot_built"
  )
})

test_that("trajectory informs when interpolate = FALSE is ignored", {
  set.seed(51)
  d <- create_test_tf_mv_data(d = 2, n_funcs = 1, n_points = 11)
  expect_message(
    ggplot_build(
      tf_ggplot(d, aes(tf = mv), interpolate = FALSE) + geom_path()
    ),
    "ignored"
  )
})

# Errors ----------------------------------------------------------------------

test_that("trajectory with d != 2 errors", {
  set.seed(6)
  d <- create_test_tf_mv_data(d = 3, n_funcs = 2, n_points = 11)
  expect_error(
    ggplot_build(
      tf_ggplot(d, aes(tf = mv), type = "trajectory") + geom_path()
    ),
    "exactly 2"
  )
})

test_that("trajectory errors with geom_line because it sorts by x", {
  set.seed(61)
  d <- create_test_tf_mv_data(d = 2, n_funcs = 1, n_points = 21)
  expect_error(
    ggplot_build(tf_ggplot(d, aes(tf = mv)) + geom_line()),
    "geom_path"
  )
})

test_that("tf_mv combined with another tf aesthetic errors", {
  set.seed(7)
  d <- create_test_tf_mv_data(d = 2, n_funcs = 2, n_points = 11)
  expect_error(
    ggplot_build(
      tf_ggplot(d, aes(tf = mv, tf_ymin = tf_component(mv, 1))) + geom_line()
    ),
    "cannot be combined"
  )
})

test_that("tf_mv mapped to a non-tf aesthetic errors", {
  set.seed(8)
  d <- create_test_tf_mv_data(d = 2, n_funcs = 2, n_points = 11)
  expect_error(
    ggplot_build(tf_ggplot(d, aes(tf_x = mv)) + geom_path()),
    "must be mapped with"
  )
})

# autoplot / autolayer --------------------------------------------------------

test_that("autoplot.tf_mv d == 2 is a trajectory tf_ggplot", {
  set.seed(9)
  mv <- create_test_tf_mv(d = 2, n_funcs = 4)
  p <- autoplot(mv)
  expect_true(is_tf_ggplot(p))
  built <- ggplot_build(p)
  expect_s3_class(built$plot$layers[[1]]$geom, "GeomPath")
  expect_equal(built$plot$labels$x, "x")
  expect_equal(built$plot$labels$y, "y")
})

test_that("autoplot.tf_mv d > 2 facets by component", {
  set.seed(91)
  mv <- create_test_tf_mv(d = 3, n_funcs = 3)
  built <- ggplot_build(autoplot(mv))
  expect_s3_class(built$plot$facet, "FacetWrap")
})

test_that("autolayer.tf_mv works with plain ggplot() and tf_ggplot()", {
  set.seed(92)
  mv2 <- create_test_tf_mv(d = 2, n_funcs = 3)
  mv3 <- create_test_tf_mv(d = 3, n_funcs = 2)
  expect_s3_class(ggplot_build(ggplot() + autolayer(mv2)), "ggplot_built")
  expect_s3_class(ggplot_build(tf_ggplot() + autolayer(mv2)), "ggplot_built")
  expect_s3_class(
    ggplot_build(ggplot() + autolayer(mv3) + facet_wrap(~.component)),
    "ggplot_built"
  )
})
