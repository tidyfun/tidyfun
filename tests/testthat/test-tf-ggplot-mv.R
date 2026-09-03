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
  # disjoint component arg ranges -> tf 0.5.0 widens the shared domain
  expect_warning(mv <- tfd_mv(list(x = cx, y = cy)), "Widening domain")
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

# tfb_mv ----------------------------------------------------------------------

test_that("tfb_mv columns plot in trajectory and facet mode", {
  set.seed(10)
  arg <- seq(0, 1, length.out = 21)
  mvb <- tfb_mv(list(
    x = tfb(tf_rgp(3, arg = arg), k = 8, verbose = FALSE),
    y = tfb(tf_rgp(3, arg = arg), k = 8, verbose = FALSE)
  ))
  d <- data.frame(id = 1:3)
  d$mv <- mvb

  b_traj <- ggplot_build(tf_ggplot(d, aes(tf = mv)) + geom_path())$data[[1]]
  expect_equal(length(unique(b_traj$group)), 3)
  expect_equal(nrow(b_traj), 3 * 21)

  b_facet <- ggplot_build(
    tf_ggplot(d, aes(tf = mv), type = "facet") + geom_line()
  )$data[[1]]
  expect_equal(length(unique(b_facet$group)), 3 * 2)
  expect_equal(nrow(b_facet), 3 * 2 * 21)
})

# Single-component tf_mv --------------------------------------------------------

test_that("d = 1 tf_mv defaults to facet display with one group per curve", {
  set.seed(101)
  d <- data.frame(id = 1:3)
  d$mv <- tfd_mv(list(a = tf_rgp(3, 11L)))
  b <- ggplot_build(tf_ggplot(d, aes(tf = mv)) + geom_line())$data[[1]]
  expect_equal(length(unique(b$group)), 3)
  expect_equal(nrow(b), 3 * 11)
})

# Zero-length tf_mv -------------------------------------------------------------

test_that("zero-length tf_mv columns build empty plots without error", {
  set.seed(102)
  d <- create_test_tf_mv_data(d = 2, n_funcs = 3, n_points = 11)
  d0 <- d[0, , drop = FALSE]

  b_traj <- ggplot_build(tf_ggplot(d0, aes(tf = mv)) + geom_path())
  expect_s3_class(b_traj, "ggplot_built")
  expect_equal(nrow(b_traj$data[[1]]), 0)

  b_facet <- ggplot_build(
    tf_ggplot(d0, aes(tf = mv), type = "facet") + geom_line()
  )
  expect_s3_class(b_facet, "ggplot_built")
  expect_equal(nrow(b_facet$data[[1]]), 0)
})

# Univariate-only geoms abort informatively ------------------------------------

test_that("tf_mv into geom_spaghetti/geom_meatballs aborts informatively", {
  set.seed(103)
  d <- create_test_tf_mv_data(d = 2, n_funcs = 3, n_points = 11)
  expect_error(
    ggplot_build(ggplot(d, aes(y = mv)) + geom_spaghetti()),
    "does not support multivariate"
  )
  expect_error(
    ggplot_build(ggplot(d, aes(y = mv)) + geom_meatballs()),
    "does not support multivariate"
  )
})

test_that("tf_mv into gglasagna aborts informatively", {
  set.seed(104)
  d <- create_test_tf_mv_data(d = 2, n_funcs = 3, n_points = 11)
  expect_error(gglasagna(d, mv), "does not support multivariate")
})

test_that("tf_mv into geom_fboxplot aborts informatively", {
  set.seed(105)
  d <- create_test_tf_mv_data(d = 2, n_funcs = 5, n_points = 11)
  expect_error(
    ggplot_build(tf_ggplot(d, aes(tf = mv)) + geom_fboxplot()),
    "does not support multivariate"
  )
  expect_error(
    ggplot_build(ggplot(d, aes(tf = mv)) + geom_fboxplot()),
    "does not support multivariate"
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

test_that("autoplot.tf_mv honours type = 'facet' override for d == 2", {
  set.seed(93)
  mv <- create_test_tf_mv(d = 2, n_funcs = 4)
  built <- ggplot_build(autoplot(mv, type = "facet"))
  expect_s3_class(built$plot$facet, "FacetWrap")
  expect_s3_class(built$plot$layers[[1]]$geom, "GeomLine")
  expect_equal(length(unique(built$data[[1]]$PANEL)), 2)
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

test_that("covariates named like generated mv columns don't break the build", {
  set.seed(31)
  d <- tibble::tibble(
    mv = tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3))),
    .mv_x = c("a", "b", "c")
  )
  p <- tf_ggplot(d, aes(tf = mv, colour = .mv_x), type = "facet") + geom_line()
  b <- ggplot_build(p)
  expect_identical(length(unique(b$data[[1]]$colour)), 3L)
  # trajectory mode with the colliding covariate
  p2 <- tf_ggplot(d, aes(tf = mv, colour = .mv_x)) + geom_path()
  expect_s3_class(ggplot_build(p2), "ggplot_built")
  # .component itself is reserved and errors informatively
  d$.component <- 1:3
  expect_error(
    ggplot_build(tf_ggplot(d, aes(tf = mv), type = "facet") + geom_line()),
    "reserved"
  )
})

# Colouring trajectories by argument ------------------------------------------

test_that("trajectory long data exposes .arg for aesthetics", {
  set.seed(200)
  d <- create_test_tf_mv_data(d = 2, n_funcs = 3, n_points = 11)
  p <- tf_ggplot(d, aes(tf = mv, colour = .arg)) + geom_path()
  b <- ggplot_build(p)
  ld <- layer_data(p)
  # colour varies within each curve: one distinct colour per grid point
  expect_equal(length(unique(ld$colour)), 11)
  expect_equal(length(unique(ld$colour[ld$group == ld$group[1]])), 11)
  # the continuous colour scale is trained on the arg grid
  expect_equal(range(b$plot$scales$get_scales("colour")$range$range), c(0, 1))
  # the .arg column is the exact evaluation grid, ordered per curve
  built_layer <- b$plot@layers[[1]]
  expect_true(".arg" %in% names(built_layer$data))
  expect_equal(
    built_layer$data$.arg,
    rep(seq(0, 1, length.out = 11), 3)
  )
})

test_that(".arg is available in facet, planar tf_x/tf_y and univariate layers", {
  set.seed(201)
  d <- create_test_tf_mv_data(d = 3, n_funcs = 2, n_points = 11)
  p <- tf_ggplot(d, aes(tf = mv, colour = .arg), type = "facet") + geom_line()
  ld <- layer_data(p)
  expect_equal(length(unique(ld$colour)), 11)

  d2 <- tibble::tibble(fx = tf_rgp(2, 11L), fy = tf_rgp(2, 11L))
  p2 <- tf_ggplot(d2, aes(tf_x = fx, tf_y = fy, colour = .arg)) + geom_path()
  ld2 <- layer_data(p2)
  expect_equal(length(unique(ld2$colour)), 11)
  expect_equal(length(unique(ld2$group)), 2)

  d3 <- create_test_tf_data(n_funcs = 2, n_points = 11)
  p3 <- tf_ggplot(d3, aes(tf = func, colour = .arg)) + geom_line()
  ld3 <- layer_data(p3)
  expect_equal(length(unique(ld3$colour)), 11)
  # .arg is a copy of the layer's arg grid (mapped to x here)
  expect_equal(ld3$x, ggplot_build(p3)$plot@layers[[1]]$data$.arg)
})

test_that("a user column named .arg errors informatively", {
  set.seed(202)
  d <- create_test_tf_mv_data(d = 2, n_funcs = 3, n_points = 11)
  d$.arg <- 1:3
  expect_error(
    ggplot_build(tf_ggplot(d, aes(tf = mv)) + geom_path()),
    "reserved"
  )
  d3 <- create_test_tf_data(n_funcs = 3, n_points = 11)
  d3$.arg <- 1:3
  expect_error(
    ggplot_build(tf_ggplot(d3, aes(tf = func)) + geom_line()),
    "reserved"
  )
})

test_that("autoplot/autolayer colour trajectories by arg on request", {
  set.seed(203)
  mv <- create_test_tf_mv(d = 2, n_funcs = 3, n_points = 11)

  p <- autoplot(mv, colour_by_arg = TRUE)
  expect_true(is_tf_ggplot(p))
  b <- ggplot_build(p)
  expect_s3_class(b$plot@layers[[1]]$geom, "GeomPath")
  expect_equal(length(unique(b$data[[1]]$colour)), 11)
  expect_equal(b$plot@labels$colour, "arg")
  # explicit labs() still win
  b2 <- ggplot_build(autoplot(mv, colour_by_arg = TRUE) + labs(colour = "t"))
  expect_equal(b2$plot@labels$colour, "t")
  # default: no colour mapping
  b0 <- ggplot_build(autoplot(mv))
  expect_equal(length(unique(b0$data[[1]]$colour)), 1)

  bl <- ggplot_build(ggplot() + autolayer(mv, colour_by_arg = TRUE))
  expect_equal(length(unique(bl$data[[1]]$colour)), 11)
  bl2 <- ggplot_build(tf_ggplot() + autolayer(mv, colour_by_arg = TRUE))
  expect_equal(length(unique(bl2$data[[1]]$colour)), 11)
})

test_that("colour_by_arg is rejected for facet displays and bad input", {
  set.seed(204)
  mv2 <- create_test_tf_mv(d = 2, n_funcs = 2, n_points = 11)
  mv3 <- create_test_tf_mv(d = 3, n_funcs = 2, n_points = 11)
  expect_error(autoplot(mv3, colour_by_arg = TRUE), "trajectory")
  expect_error(autoplot(mv2, type = "facet", colour_by_arg = TRUE), "trajectory")
  expect_error(autolayer(mv3, colour_by_arg = TRUE), "trajectory")
  expect_error(autoplot(mv2, colour_by_arg = "yes"), "TRUE")
})
