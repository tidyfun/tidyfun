context("depth measures")

grid <- round(seq(0, 10, l = 11), 3)
lin <- -3:3 * tfd(.1 * grid, grid)
parallel <- -3:3 + tfd(0*grid, grid)

spike_regular <- c(parallel, tfd(100 * (grid == 10), grid))
spike_irregular <- c(-3:3 + tfd(c(0*grid, 0), c(grid, 20)), 
  tfd(100 * (c(grid, 20) == 20), c(grid, 20)))
na <- 1*NA + lin[1]

lin_irreg <- {
  m <- as.matrix(lin)
  m[cbind(2:7, 2:7)] <- NA
  as.tfd(m, evaluator = approx_linear)
}

lin_b <- tfb(lin, verbose = FALSE)

test_that("MBD works", {
  ranks <- c(1.5, 3.5, 5.5, 7, 5.5, 3.5, 1.5)
  expect_equivalent(rank(depth(lin, depth = "MBD")), ranks)
  expect_equivalent(rank(depth(parallel, depth = "MBD")), ranks)
  expect_equivalent(rank(depth(lin_irreg, depth = "MBD")), ranks)
  expect_equivalent(rank(depth(lin_b, depth = "MBD")), ranks)
  # weighting by interval length:
  # increases importance of last point -> lower depth
  expect_true(tail(depth(spike_regular), 1) > tail(depth(spike_irregular), 1))
})

test_that("median works", {
  expect_true(median(lin, depth = "MBD") == lin[4])
  expect_true(median(parallel, depth = "MBD") == parallel[4])
  expect_true(is.na(median(c(na, lin))))
  expect_true(median(c(na, lin), na.rm = TRUE) == median(lin))
  expect_true(median(lin, depth = "pointwise") == median(lin))
  expect_warning(median(lin[1:2]), "2 observations")
})

