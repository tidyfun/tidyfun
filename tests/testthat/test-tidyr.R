context("gathering & (un)nesting")

data <- refund::DTI[1:5, ]
d1 <- data.frame(data$cca)
d2 <- data.frame(id = data$ID, data$cca)
cca <- tfd(data$cca)

test_that("tf_gather works", {
  expect_is(tf_gather(d1)$cca, "tfd")
  expect_message(tf_gather(d1)$cca, "cca")
  expect_equal(tf_gather(d1)$cca, cca)
  expect_equivalent(tf_gather(d1)$cca, tf_gather(data[, 1:8], cca)$cca)

  expect_equal(tf_gather(d2, -1)$cca, cca)
  expect_equal(tf_gather(d2, -1)$id, d2$id)
  expect_equal(tf_gather(d2, -1, key = "nuhnuh")$nuhnuh, tf_gather(d2, -1)$cca)

  expect_equal(tf_gather(d2, -id)$cca, tf_gather(d2, -1)$cca)
  expect_equal(tf_gather(d2, starts_with("cca"))$cca, tf_gather(d2, -1)$cca)

  expect_equal(
    attr(tf_gather(d1, evaluator = tf_approx_spline)$cca, "evaluator_name"),
    "tf_approx_spline"
  )
})

test_that("tf_spread works", {
  d <- tibble(g = 1:3, f = tf_rgp(3, 11L))
  expect_equivalent(
    tf_spread(d, f, sep = NULL)[, -1],
    as.data.frame(as.matrix(d$f))
  )
  expect_equivalent(tf_spread(d, f), tf_spread(d, -g))
  expect_equivalent(tf_spread(d, f), tf_spread(d))
  expect_equivalent(
    tf_spread(d, f, arg = seq(0, 1, l = 20), sep = NULL)[, -1],
    as.data.frame(d$f[, seq(0, 1, l = 20), interpolate = TRUE])
  )
  d$fb <- tfb(tf_rgp(3, 11L))
  expect_error(tf_spread(d), "More than one")
  expect_equivalent(
    tf_spread(d, fb, sep = NULL)[, - (1:2)],
    as.data.frame(as.matrix.tfb(d$fb))
  )
  d$fi <- tf_jiggle(tf_rgp(3, 11L))
  expect_error(tf_spread(d, fi), "need explicit <arg>")
  expect_equivalent(
    tf_spread(d, fi, arg = seq(0, 1, l = 20), sep = NULL)[, -(1:3)],
    as.data.frame(as.matrix(d$fi, arg = seq(0, 1, l = 20), interpolate = TRUE))
  )
})


test_that("tf_nest works", {
  f1 <- tf_rgp(3, 11L)
  f2 <- tf_rgp(3, 11L)
  data <- inner_join(as.data.frame(f1), as.data.frame(f2), by = c("id", "arg"))
  expect_equivalent(tf_nest(data)$value.x, f1)
  expect_equivalent(tf_nest(data)$value.y, f2)
  expect_equal(names(tf_nest(data, value.x:value.y)), names(tf_nest(data)))
  expect_equivalent(names(tf_nest(data, -(1:2))), names(tf_nest(data)))
  expect_error(tf_nest(data, resolution = c(.01, .4, .4)))

  g <- rnorm(3)
  data <- bind_cols(data, g = rep(g, e = tf_count(f1)))
  expect_equal(tf_nest(data, value.x:value.y)$g, g)
  data <- bind_cols(data, f = rep(rnorm(nrow(data))))
  expect_error(tf_nest(data, value.x:value.y), "Can't nest")
})

# weird scoping problem going on -- fixed by assigning
# data to tf_evaluate as evaluated object instead of quoted for now,
# otherwise tfdata is not found inside test environments (related to testthat/#266?)
test_that("tf_unnest works", {
  set.seed(121211)
  f1 <- tf_rgp(3, 11L)
  f2 <- tf_rgp(3, 11L)
  data <- inner_join(as.data.frame(f1), as.data.frame(f2), by = c("id", "arg"))
  tfdata <- tf_nest(data)
  expect_equal(NCOL(tf_unnest(tfdata, cols = c(value.x, value.y), try_dropping = FALSE)), 5)
  expect_equivalent(as.matrix(tf_unnest(tfdata, cols = c(value.x, value.y), try_dropping = TRUE)[2:4]), 
                    as.matrix(data[,2:4]))
  expect_message(tf_unnest(tfdata, cols = c(value.x, value.y), try_dropping = TRUE), 
                 "Duplicate column")
  expect_message(tf_unnest(tfdata, cols = c(value.x, value.y), try_dropping = TRUE), 
                 "Renamed")
  expect_is(tf_unnest(tfdata, value.x)$value.y, 
            "tfd")
})
