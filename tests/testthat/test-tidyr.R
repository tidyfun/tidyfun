context("gathering & (un)nesting")

data <- refund::DTI[1:5,]
d1 <- data.frame(data$cca)
d2 <- data.frame(id = data$ID, data$cca)
cca <- tfd(data$cca)

test_that("tf_gather works", {
  expect_is(tf_gather(d1)$cca, "tfd")
  expect_message(tf_gather(d1)$cca, "cca")
  expect_equal(tf_gather(d1)$cca, cca)
  
  expect_equal(tf_gather(d2, -1)$cca, cca)
  expect_equal(tf_gather(d2, -1)$id, d2$id)
  expect_equal(tf_gather(d2, -1, key = "nuhnuh")$nuhnuh, tf_gather(d2, -1)$cca)
  
  expect_equal(tf_gather(d2, -id)$cca, tf_gather(d2, -1)$cca)
  expect_equal(tf_gather(d2, matches("cca"))$cca, tf_gather(d2, -1)$cca)
  
  expect_equal(
    attr(tf_gather(d1, evaluator = approx_spline)$cca, "evaluator_name"), 
    "approx_spline")
})
