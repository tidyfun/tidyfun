source(system.file("testdata", "make-test-data.R", package = "tidyfun"))

context("tf_evaluate for tf")

test_that("tf_evaluate.tfd works for regular", {
  expect_identical(tf_evaluate(smoo),
                   tf_evaluations(smoo))
  expect_identical(tf_evaluate(smoo, arg = tf_arg(smoo)),
                   tf_evaluations(smoo))
  expect_identical(tf_evaluate(smoo, arg = 0.5),
                   tf_evaluate(smoo, 0.5))
  expect_error(tf_evaluate(smoo, 0.5, a), "too many")
})

test_that("tf_evaluate.tfd works for irregular", {
  expect_identical(tf_evaluate(irr),
                   tf_evaluations(irr))
  expect_identical(tf_evaluate(irr, arg = tf_arg(irr)),
                   tf_evaluations(irr))
  expect_identical(tf_evaluate(irr, arg = 0.5),
                   tf_evaluate(irr, 0.5))
  expect_error(tf_evaluate(irr, 0.5, a), "too many")
})

smoo_tfb <- tfb(smoo, penalized = FALSE, verbose = FALSE)

test_that("tf_evaluate.tfb works", {
  expect_identical(tf_evaluate(smoo_tfb),
                   tf_evaluations(smoo_tfb))
  expect_equal(tf_evaluate(smoo_tfb, arg = tf_arg(smoo_tfb)),
                   tf_evaluations(smoo_tfb))
  expect_equal(tf_evaluate(smoo_tfb, arg = 0.5),
                   tf_evaluate(smoo_tfb, 0.5))
  expect_error(tf_evaluate(smoo_tfb, 0.5, a), "too many")
})

context("tf_evaluate for data frames")

d <- tibble(a = tf_rgp(3), b = tf_rgp(3)) 

test_that("tf_evaluate.data.frame basically works", {
  da <- d$a
  expect_identical(tf_evaluate(da)[["a"]],
                   da[["a"]][ , tf_arg(d$a), matrix = FALSE])
})

test_that("tf_evaluate.data.frame interface works", {
  expect_identical(tf_evaluate(d), 
                   tf_evaluate(d, a, b))
  expect_identical(tf_evaluate(d, a), 
                   tf_evaluate(d, -b))
  expect_identical(tf_evaluate(d, a, arg = seq(0, 1, l = 11))[["a"]], 
                   d[["a"]][ , seq(0, 1, l = 11), matrix = FALSE])
  expect_identical(tf_evaluate(d, b, arg = seq(0, 1, l=11)), 
                   tf_evaluate(d, arg = seq(0, 1, l = 11), b))
})

