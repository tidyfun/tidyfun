d <- tibble(a = tf_rgp(3), b = tf_rgp(3))

test_that("tf_evaluate.data.frame basically works", {
  da <- d$a
  expect_identical(
    tf_evaluate(da)[["a"]], da[["a"]][, tf_arg(d$a), matrix = FALSE]
  )
})

test_that("tf_evaluate.data.frame interface works", {
  expect_identical(tf_evaluate(d), tf_evaluate(d, a, b))
  expect_identical(tf_evaluate(d, a), tf_evaluate(d, -b))
  expect_identical(
    tf_evaluate(d, a, arg = seq(0, 1, l = 11))[["a"]],
    d[["a"]][, seq(0, 1, l = 11), matrix = FALSE]
  )
  expect_identical(
    tf_evaluate(d, b, arg = seq(0, 1, l = 11)),
    tf_evaluate(d, arg = seq(0, 1, l = 11), b)
  )
})
