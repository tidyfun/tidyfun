context("calculus")

set.seed(12)
g <- 201
from <- -3.5
to <- 3.5
domain <- c(from, to)
grid <- seq(from, to, l = g)
dgrid <- seq(from + 0.5, to - 0.5, l = 50)

eval_irreg <- function(expression, g, domain) {
  args <- unique(round(sort(runif(g, domain[1],  domain[2])), 3))
  f <- eval(expression, list(x = args))
  feval(f, argvals = args)
}

cubic <- feval(grid^3, grid) 
square <- 3 * feval(grid^2, grid)
lin <- 6 * feval(grid, grid)
cubic_irreg <- eval_irreg(expression(x^3), g, domain)
cubic_b <- fbase(cubic, k = 45, bs = "tp", verbose = FALSE)
square_irreg <- eval_irreg(expression(3 * x^2), g, domain)
square_b <- fbase(square, k = 45, bs = "tp", verbose = FALSE)

test_that("basic derivatives work", {
  dgrid <- seq(from + 0.5, to - 0.5, l = 50)
  
  expect_equivalent(deriv(cubic)[,dgrid], square[,dgrid], tolerance = .1)
  expect_equivalent(deriv(cubic_irreg)[,dgrid], square[,dgrid], tolerance = .1)
  expect_equivalent(deriv(cubic_b)[,dgrid], square[,dgrid], tolerance = .1)
  expect_equivalent(deriv(cubic, 2)[,dgrid], lin[,dgrid], tolerance = .1)
  expect_equivalent(deriv(cubic_irreg, 2)[,dgrid], lin[,dgrid], tolerance = .1)
  expect_equivalent(deriv(cubic_b, 2)[,dgrid], lin[,dgrid], tolerance = .1)
})

test_that("basic definite integration works", {
  expect_equivalent(integrate(square), to^3 - from^3, tolerance = .1)
  expect_equivalent(integrate(square_irreg), to^3 - from^3, tolerance = .1)
  expect_equivalent(integrate(square_b), to^3 - from^3, tolerance = .1)
})

test_that("basic antiderivatives work", {
  expect_equivalent(integrate(square, definite = FALSE)[,dgrid], 
    cubic[,dgrid] - from^3, tolerance = .1)
  expect_equivalent(integrate(square_irreg, definite = FALSE)[,dgrid], 
    cubic[,dgrid] - from^3, tolerance = .1)
  expect_equivalent(integrate(square_b, definite = FALSE)[,dgrid], 
    cubic[,dgrid]  - from^3, tolerance = .1)
})

test_that("deriv & integrate are reversible (approximately)", {
  set.seed(1337)
  f <- rgp(10, argvals = grid, nugget = 0)
  f <- f - f[, grid[1]] # start at  0
  f2 <- integrate(deriv(f), definite = FALSE)
  f3 <- deriv(integrate(f, definite = FALSE))
  expect_equivalent(f[,dgrid], f2[,dgrid], tolerance = .1)
  expect_equivalent(f[,dgrid], f3[,dgrid], tolerance = .1)
  #plot(f); lines(f2, col = 2, lty =2); lines(f3, col = 3, lty = 3)

  expect_error(integrate(deriv(fbase(f, verbose = FALSE)), definite = FALSE), 
    "previously")
  
  f_pc <- fpcbase(f, smooth = FALSE, verbose = FALSE)
  f_pc2 <- integrate(deriv(f_pc), definite = FALSE)
  f_pc3 <- deriv(integrate(f_pc, definite = FALSE))
  expect_equivalent(f_pc[,dgrid], f_pc2[,dgrid], tolerance = .1)
  expect_equivalent(f_pc[,dgrid], f_pc3[,dgrid], tolerance = .1)
  #plot(f_pc); lines(f_pc2, col = 2, lty =2); lines(f_pc3, col = 3, lty = 3)
  expect_equivalent(integrate(f_pc), integrate(f), tolerance = .1)
})


