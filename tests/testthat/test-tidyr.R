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
  expect_equal(tf_gather(d2, starts_with("cca"))$cca, tf_gather(d2, -1)$cca)
  
  expect_equal(
    attr(tf_gather(d1, evaluator = approx_spline)$cca, "evaluator_name"), 
    "approx_spline")
})

test_that("tf_nest works", {
  f1 <- rgp(3, 11L)
  f2 <- rgp(3, 11L)
  data <- inner_join(as.data.frame(f1), as.data.frame(f2), by = c("id", "arg"))
  expect_equivalent(tf_nest(data)$value.x, f1)
  expect_equivalent(tf_nest(data)$value.y, f2)
  expect_equal(names(tf_nest(data, value.x:value.y)),names(tf_nest(data)))
  expect_equivalent(names(tf_nest(data, -(1:2))), names(tf_nest(data)))
  g <- rnorm(3)
  data <- bind_cols(data, g = rep(g, e = n_evaluations(f1)))
  expect_equal(tf_nest(data, value.x:value.y)$g, g)
  data <- bind_cols(data, f = rep(rnorm(nrow(data))))
  expect_error(tf_nest(data, value.x:value.y), "Columns f are not constant")
})

test_that("tf_unnest works", {
  f1 <- rgp(3, 11L)
  f2 <- rgp(3, 11L)
  data <- inner_join(as.data.frame(f1), as.data.frame(f2), by = c("id", "arg"))
  tfdata <- tf_nest(data)
  expect_true(all(tf_unnest(tfdata)[] == data))
  expect_message(tf_unnest(tfdata), "Duplicate columns")
  expect_message(tf_unnest(tfdata), "Renamed")
  expect_is(tf_unnest(tfdata, value.x, .preserve = value.y)$value.y, "tfd")
})
