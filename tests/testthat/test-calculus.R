context("calculus")

set.seed(12)
g <- 201
from <- -3.5
to <- 3.5
domain <- c(from, to)
grid <- seq(from, to, l = g)
dgrid <- seq(from + 0.5, to - 0.5, l = 50)

eval_irreg <- function(expression, g, domain) {
  args <- unique(round(sort(runif(g, domain[1], domain[2])), 3))
  f <- eval(expression, list(x = args))
  tfd(f, arg = args)
}

cubic <- tfd(grid^3, grid)
square <- 3 * tfd(grid^2, grid)
lin <- 6 * tfd(grid, grid)
cubic_irreg <- eval_irreg(expression(x^3), g, domain)
cubic_b <- tfb(cubic, k = 45, bs = "tp", verbose = FALSE)
square_irreg <- eval_irreg(expression(3 * x^2), g, domain)
square_b <- tfb(square, k = 45, bs = "tp", verbose = FALSE)

test_that("basic derivatives work", {
  dgrid <- seq(from + 0.5, to - 0.5, l = 50)

  expect_equivalent(tf_derive(cubic)[, dgrid], square[, dgrid], tolerance = .1)
  expect_equivalent(tf_derive(cubic_irreg)[, dgrid], square[, dgrid], tolerance = .1)
  expect_equivalent(tf_derive(cubic_b)[, dgrid], square[, dgrid], tolerance = .1)
  expect_equivalent(tf_derive(cubic, order = 2)[, dgrid], lin[, dgrid], tolerance = .1)
  expect_equivalent(tf_derive(cubic_irreg, order = 2)[, dgrid], lin[, dgrid], tolerance = .1)
  expect_equivalent(tf_derive(cubic_b, order = 2)[, dgrid], lin[, dgrid], tolerance = .1)
})

test_that("basic definite integration works", {
  expect_equivalent(tf_integrate(square), to^3 - from^3, tolerance = .1)
  expect_equivalent(tf_integrate(square_irreg), to^3 - from^3, tolerance = .1)
  expect_equivalent(tf_integrate(square_b), to^3 - from^3, tolerance = .1)
})

test_that("basic antiderivatives work", {
  expect_equivalent(tf_integrate(square, definite = FALSE)[, dgrid],
    cubic[, dgrid] - from^3,
    tolerance = .1
  )
  expect_equivalent(tf_integrate(square_irreg, definite = FALSE)[, dgrid],
    cubic[, dgrid] - from^3,
    tolerance = .1
  )
  expect_equivalent(tf_integrate(square_b, definite = FALSE)[, dgrid],
    cubic[, dgrid] - from^3,
    tolerance = .1
  )
})

test_that("deriv & tf_integrate are reversible (approximately)", {
  set.seed(1337)
  f <- tf_rgp(10, arg = grid, nugget = 0)
  f <- f - f[, grid[1]] # start at  0
  f2 <- tf_integrate(tf_derive(f), definite = FALSE)
  f3 <- tf_derive(tf_integrate(f, definite = FALSE))
  expect_equivalent(f[, dgrid], f2[, dgrid], tolerance = .1)
  expect_equivalent(f[, dgrid], f3[, dgrid], tolerance = .1)

  expect_error(
    tf_integrate(tf_derive(tfb(f, verbose = FALSE)), definite = FALSE),
    "previously"
  )

  f_pc <- tfb_fpc(f[1:3, seq(tf_domain(f)[1], tf_domain(f)[2], l = 101)],
    smooth = FALSE, verbose = FALSE
  )
  f_pc2 <- tf_integrate(tf_derive(f_pc), definite = FALSE)
  f_pc3 <- tf_derive(tf_integrate(f_pc, definite = FALSE))
  expect_equivalent(f_pc[, dgrid], f_pc2[, dgrid], tolerance = .1)
  expect_equivalent(f_pc[, dgrid], f_pc3[, dgrid], tolerance = .1)
  expect_equivalent(tf_integrate(f_pc), tf_integrate(f[1:3]), tolerance = .01)
})
