context("where")
test_that("where works", {
  lin <- 1:2 * tfd(seq(-1, 1,l = 11), seq(-1, 1, l = 11))
  expect_equal(where(lin, value %inr% c(-1, -.5)), 
    list(c(-1.0, -0.8, -0.6), c(-0.4)))
  expect_equal(where(lin, value > 0, "first"),
    c(0.2, 0.2))
  expect_equal(where(lin, value < 0, "last"),
    c(-0.2, -0.2))
  expect_equivalent(where(lin, value <= 0, "range"),
    data.frame(begin = -1, end = c(0, 0)))
  expect_identical(where(lin, value < -1.5, "any"),
    c(FALSE, TRUE))
  expect_identical(where(lin, value < -1.5, "any"),
    anywhere(lin, value < -1.5))
  expect_identical(where(lin, value < -2),
    list(numeric(0), numeric(0)))
})
