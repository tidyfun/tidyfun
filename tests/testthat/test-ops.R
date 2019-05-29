test_that("fun_op keeps names", {
  x <- tf_rgp(3)
  xn <- x; names(xn) <- letters[1:3]
  
  expect_equal(names(x - xn), names(x))
  expect_equal(names(xn - x), names(xn))
  expect_equal(names(xn - mean(x)), names(xn))
  expect_equal(names(mean(xn) - xn), names(xn - mean(x)))
})
