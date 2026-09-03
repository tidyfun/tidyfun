# Tests for tf_phaseplane() and phase-plane / colour-by-arg trajectory plots

library(ggplot2)

# smooth test functions on a fine grid: sinusoids sin(2 pi k t)
make_sinusoids <- function(k = 1:3, n_points = 101) {
  arg <- seq(0, 1, length.out = n_points)
  tfd(t(sapply(k, \(kk) sin(2 * pi * kk * arg))), arg = arg)
}

test_that("tf_phaseplane returns a 2-component tf_mv of derivatives", {
  f <- make_sinusoids()
  pp <- tf_phaseplane(f)

  expect_s3_class(pp, "tfd_mv")
  expect_equal(tf_ncomp(pp), 2)
  expect_equal(names(tf_components(pp)), c("D1", "D2"))
  expect_equal(length(pp), length(f))
  expect_equal(tf_component(pp, 1), tf_derive(f, order = 1))
  expect_equal(tf_component(pp, 2), tf_derive(f, order = 2))
})

test_that("tf_phaseplane derivatives are accurate for smooth input", {
  f <- make_sinusoids(k = 1, n_points = 201)
  pp <- tf_phaseplane(f)
  arg <- seq(0.1, 0.9, by = 0.1)
  d1 <- unlist(tf_evaluate(tf_component(pp, 1), arg = arg))
  d2 <- unlist(tf_evaluate(tf_component(pp, 2), arg = arg))
  expect_equal(d1, 2 * pi * cos(2 * pi * arg), tolerance = 1e-2)
  expect_equal(d2, -(2 * pi)^2 * sin(2 * pi * arg), tolerance = 1e-2)
})

test_that("tf_phaseplane honours order, incl. 0 for the function itself", {
  f <- make_sinusoids()
  pp <- tf_phaseplane(f, order = c(0, 1))
  expect_equal(names(tf_components(pp)), c("D0", "D1"))
  expect_equal(tf_component(pp, 1), f)
  expect_equal(tf_component(pp, 2), tf_derive(f))
  # order of axes follows `order`
  pp_rev <- tf_phaseplane(f, order = c(2, 1))
  expect_equal(names(tf_components(pp_rev)), c("D2", "D1"))
  expect_equal(tf_component(pp_rev, 2), tf_component(tf_phaseplane(f), 1))
})

test_that("tf_phaseplane evaluates on a user-supplied arg grid", {
  f <- make_sinusoids()
  grid <- seq(0, 1, length.out = 26)
  pp <- tf_phaseplane(f, arg = grid)
  expect_equal(tf_arg(pp), grid)
})

test_that("tf_phaseplane keeps tfb input in basis representation", {
  f <- make_sinusoids()
  fb <- tfb(f, k = 15, verbose = FALSE)
  pp <- tf_phaseplane(fb)
  expect_s3_class(pp, "tfb_mv")
  expect_equal(names(tf_components(pp)), c("D1", "D2"))
  # order = 0 keeps the (undifferentiated) basis object as first component
  pp0 <- tf_phaseplane(fb, order = c(0, 1))
  expect_s3_class(pp0, "tfb_mv")
  expect_equal(tf_component(pp0, 1), fb)
  # fpc bases work too
  fpc <- tfb(f, basis = "fpc", verbose = FALSE)
  expect_s3_class(tf_phaseplane(fpc), "tfb_mv")
})

test_that("tf_phaseplane works for irregular data (via tfd_mv)", {
  set.seed(1)
  f <- tf_sparsify(make_sinusoids(), dropout = 0.3)
  expect_s3_class(f, "tfd_irreg")
  pp <- suppressMessages(tf_phaseplane(f))
  expect_s3_class(pp, "tfd_mv")
  expect_equal(length(pp), length(f))
})

test_that("tf_phaseplane preserves names", {
  f <- make_sinusoids()
  names(f) <- c("a", "b", "c")
  expect_equal(names(tf_phaseplane(f)), c("a", "b", "c"))
})

test_that("tf_phaseplane validates its inputs", {
  f <- make_sinusoids()
  expect_error(tf_phaseplane(1:3), "univariate")
  expect_error(tf_phaseplane(tf_phaseplane(f)), "univariate")
  expect_error(tf_phaseplane(f, order = 1), "2 non-negative integers")
  expect_error(tf_phaseplane(f, order = 1:3), "2 non-negative integers")
  expect_error(tf_phaseplane(f, order = c(-1, 1)), "2 non-negative integers")
  expect_error(tf_phaseplane(f, order = c(0.5, 1)), "2 non-negative integers")
  expect_error(tf_phaseplane(f, order = c(1, 1)), "different")
})

# plotting ---------------------------------------------------------------------

test_that("phase-plane trajectories plot with tf_ggplot and autoplot", {
  f <- make_sinusoids()
  d <- data.frame(id = factor(seq_along(f)))
  d$f <- f

  p <- tf_ggplot(d, aes(tf = tf_phaseplane(f), colour = id)) + geom_path()
  b <- ggplot_build(p)
  expect_equal(length(unique(b$data[[1]]$group)), 3)
  expect_equal(nrow(b$data[[1]]), 3 * 101)
  expect_equal(b$plot@labels$x, "D1")
  expect_equal(b$plot@labels$y, "D2")
  # x is velocity, y is acceleration
  expect_setequal(
    round(b$data[[1]]$x, 8),
    round(unlist(tf_evaluations(tf_derive(f))), 8)
  )

  pa <- autoplot(tf_phaseplane(f, order = c(0, 1)), colour_by_arg = TRUE)
  ba <- ggplot_build(pa)
  expect_equal(ba$plot@labels$x, "D0")
  expect_equal(ba$plot@labels$y, "D1")
  expect_equal(ba$plot@labels$colour, "arg")
})
