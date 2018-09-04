context("evaluate")

grid <- round(seq(0, 1, l = 21), 3)
lin <- 2 * grid
curve <- sin(3 * pi * grid)

f_lin <- tfd(data.frame(1, grid, lin))
f_curve <- tfd(data.frame(1, grid, curve))

new_grid <- round(seq(0, 1, l = 41), 3)

test_that("argval checking works", {
  expect_error(tf_evaluate(f_lin, min(grid) - 1), ">=")
  expect_error(tf_evaluate(f_lin, max(grid) + 1), "<=")
  expect_error(tf_evaluate(f_lin, 1 * NA), "missing")
  expect_error(tf_evaluate(f_lin, list(1, 2)), "length")
})

test_that("evaluator tf_approx_linear works", {
  expect_identical(
    lin,
    suppressWarnings(tf_evaluator(f_lin)(grid, arg = grid, evaluations = lin))
  )
  expect_identical(
    2 * new_grid,
    suppressWarnings(tf_evaluator(f_lin)(new_grid, arg = grid, evaluations = lin))
  )
  expect_identical(lin, tf_evaluate(f_lin, grid)[[1]])
  expect_equal(2 * new_grid, tf_evaluate(f_lin, new_grid)[[1]])
  expect_equal(curve, tf_evaluate(f_curve, new_grid)[[1]][new_grid %in% grid])
})

test_that("re-assigning & extracting evaluator works", {
  tf_evaluator(f_lin) <- tf_approx_spline
  tf_evaluator(f_curve) <- tf_approx_spline
  expect_equivalent(
    body(environment(tf_evaluator(f_lin))[["_f"]]),
    body(tf_approx_spline)
  )
  expect_equivalent(
    body(environment(tf_evaluator(f_lin))[["_f"]]),
    body(environment(tf_evaluator(f_curve))[["_f"]])
  )
})

tf_evaluator(f_lin) <- tf_approx_spline
tf_evaluator(f_curve) <- tf_approx_spline

test_that("evaluator tf_approx_spline works", {
  expect_identical(
    lin,
    suppressWarnings(tf_evaluator(f_lin)(grid, arg = grid, evaluations = lin))
  )
  expect_identical(
    2 * new_grid,
    suppressWarnings(tf_evaluator(f_lin)(new_grid, arg = grid, evaluations = lin))
  )
  expect_identical(lin, tf_evaluate(f_lin, grid)[[1]])
  expect_equal(2 * new_grid, tf_evaluate(f_lin, new_grid)[[1]])
})

test_that("memoisation works", {
  slow_tf_approx <- function(x, arg, evaluations) {
    Sys.sleep(0.2)
    tf_approx_linear(x, arg, evaluations)
  }
  tf_evaluator(f_lin) <- slow_tf_approx
  t1 <- system.time(tf_evaluate(f_lin, new_grid))[3]
  t2 <- system.time(tf_evaluate(f_lin, new_grid))[3]
  expect_true(t1 > 10 * t2)
})

test_that("multiple arg-vectors work for tfb", {
  fb <- tfb(tf_rgp(3), verbose = FALSE)
  expect_equal(
    unlist(tf_evaluate(fb, as.list(c(0, .5, 1)))),
    unlist(c(tf_evaluate(fb[1], 0), tf_evaluate(fb[2], 0.5), tf_evaluate(fb[3], 1)))
  )
})


context("resolution")

test_that("resolution finding works", {
  fi <- tfd(list(c(1, 2), c(1, 2)), arg = list(c(0, .1), c(1, 2)))
  expect_equal(attr(fi, "resolution"), 0.01)
  f <- tf_rgp(3, 101L)
  expect_equal(attr(f, "resolution"), 1e-4)
  fb <- tfb(f, verbose = FALSE)
  expect_equal(attr(fb, "resolution"), 1e-4)
})

test_that("resolution warnings work", {
  expect_error(
    tfd(list(c(1, 2), c(1, 2)), arg = list(c(0, .1), c(1, 2)), resolution = 1),
    "Non-unique arg-values"
  )
  expect_error(
    tfd(c(0, 1), arg = c(0, .1), resolution = .5),
    "Non-unique arg-values"
  )
  expect_error(
    tfb(0:10, arg = (0:10) / 10, resolution = 5, verbose = FALSE),
    "Non-unique arg-values"
  )
})

test_that("resolution works as expected", {
  f <- tfd(1:10, 1:10, resolution = .05, evaluator = tf_approx_none)
  set.seed(122)
  fi <- tf_sparsify(f)
  tf_evaluator(fi) <- tf_approx_none
  fb <- tfb(f, verbose = FALSE)

  # argvals +/- resolution/2 are not distinguished:
  expect_equivalent(f[, 1:9 + .01], f[, 1:9])
  expect_equivalent(f[, 1:9 + .0249], f[, 1:9])
  expect_true(all(is.na(f[, 1:9 + .0251])))

  expect_equivalent(fi[, 1:9 + .01], fi[, 1:9])
  expect_equivalent(fi[, 1:9 + .0249], fi[, 1:9])
  expect_true(all(is.na(fi[, 1:9 + .0251])))

  expect_equivalent(fb[, 1:9 + .01], fb[, 1:9])
  expect_equivalent(fb[, 1:9 + .0249], fb[, 1:9])
})


# resolution error when too low
# constant value for arg inside resolution
#
