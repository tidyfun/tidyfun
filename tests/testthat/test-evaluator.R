context("evaluator")

grid <- round(seq(0, 1, l = 21), 3)
lin <- 2*grid
curve <- sin(3*pi*grid)

f_lin <- tfd(data.frame(1, grid, lin))
f_curve <- tfd(data.frame(1, grid, curve))

new_grid <- round(seq(0, 1, l = 41), 3)

test_that("argval checking works", {
  expect_error(evaluate(f_lin, min(grid) - 1), ">=")
  expect_error(evaluate(f_lin, max(grid) + 1), "<=")
  expect_error(evaluate(f_lin, 1*NA), "missing")
  expect_error(evaluate(f_lin, c(1,1)), "duplicated")
  expect_error(evaluate(f_lin, list(1, 2)), "length")
})  

test_that("evaluator approx_linear works", {
  expect_identical(lin, evaluator(f_lin)(grid, arg = grid, evaluations = lin))
  expect_identical(2 * new_grid, 
    evaluator(f_lin)(new_grid, arg = grid, evaluations = lin))
  expect_identical(lin, evaluate(f_lin, grid)[[1]])
  expect_identical(2 * new_grid, evaluate(f_lin, new_grid)[[1]])
  
  expect_equivalent(curve, 
    evaluator(f_curve)(grid, arg = grid, evaluations = curve))
  expect_equal(sin(3*pi*new_grid), 
    evaluator(f_curve)(new_grid, arg = grid, evaluations = curve), 
    tolerance = 1e-1)
  expect_equal(curve, evaluate(f_curve, new_grid)[[1]][new_grid %in% grid])
})

test_that("re-assigning & extracting evaluator works", {
  evaluator(f_lin) <- approx_spline
  evaluator(f_curve) <- approx_spline
  expect_equivalent(body(environment(evaluator(f_lin))[["_f"]]), 
    body(approx_spline))
  expect_equivalent(body(environment(evaluator(f_lin))[["_f"]]), 
    body(environment(evaluator(f_curve))[["_f"]]))
})

evaluator(f_lin) <- approx_spline
evaluator(f_curve) <- approx_spline

test_that("evaluator approx_spline works", {
  expect_identical(lin, evaluator(f_lin)(grid, arg = grid, evaluations = lin))
  expect_identical(2 * new_grid, 
    evaluator(f_lin)(new_grid, arg = grid, evaluations = lin))
  expect_identical(lin, evaluate(f_lin, grid)[[1]])
  expect_identical(2 * new_grid, evaluate(f_lin, new_grid)[[1]])
  
  expect_equivalent(curve, 
    evaluator(f_curve)(grid, arg = grid, evaluations = curve))
  expect_equal(sin(3*pi*new_grid), 
    evaluator(f_curve)(new_grid, arg = grid, evaluations = curve), 
    tolerance = 1e-3)
  expect_equal(curve, evaluate(f_curve, new_grid)[[1]][new_grid %in% grid])
})

test_that("memoisation works", {
  slow_approx <- function(x, arg, evaluations) {
    Sys.sleep(0.2)
    approx_linear(x, arg, evaluations)
  }
  evaluator(f_lin) <- slow_approx
  t1 <- system.time(evaluate(f_lin, new_grid))[3]
  t2 <- system.time(evaluate(f_lin, new_grid))[3]
  expect_true(t1 > 10 * t2)
})
