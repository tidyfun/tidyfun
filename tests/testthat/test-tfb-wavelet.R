# check constructors from tfd, matrix, data.frame, list
context("tfb_wavelet constructor: basics")


set.seed(1234)
f <- function(t) 2*t + dbeta(t, 5, 7) * ifelse(t < .5, -1, 1) - .2 * dnorm(t, 0.2, 0.05)

woo <- data.frame(id = rep(1:100, each = 256), 
                  arg = rep(seq(0, 1, l = 256), 100))

woo_df <- woo %>% mutate(data = f(arg) + rnorm(nrow(woo), sd = .5))
woo_tfd <- tfd(woo_df)
woo_mat <- as.matrix(woo_tfd)
woo_tfb <- tfb_wavelet(woo_tfd, level = 2)

test_that("tfb_wavelet defaults work for all kinds of regular input", {
  for (dat in list(woo_df, woo_tfd, woo_mat)) {
    woo_ <- try(tfb_wavelet(dat, verbose = FALSE))
    expect_is(woo_, "tfb_wavelet")
    expect_equal(length(woo_), length(woo_tfd))
    expect_equivalent(tf_evaluations(woo_), tf_evaluations(woo_tfd),
                      tolerance = 1)
  }
})


test_that("tfb_wavelet works for different wavelet parameters", {
  for (i in 1:10) {
    woo_tfb <- tfb_wavelet(woo_tfd, level = 2, filter_number = i) 
    error <- mean((tf_evaluate(woo_tfb)[[1]] - filter(woo_df, id == 1)$data)^2)
    for (j in 3:6) {
      woo_tfb <- tfb_wavelet(woo_tfd, level = j, filter_number = i) 
      error_1 <- mean((tf_evaluate(woo_tfb)[[1]] - filter(woo_df, id == 1)$data)^2)
      expect_less_than(error_1, error)
      error <- error_1
    }
  }
})

test_that("tfb_wavelet independent of datatype", {
  expect_equal(tfb_wavelet(woo_tfd), tfb_wavelet(woo_df))
  expect_equal(tfb_wavelet(woo_tfd), tfb_wavelet(woo_mat))
})


context("non dyadic, non equispaced data")
irr_grid <- data.frame(id = rep(1:100, each = 230), 
                  arg = rep(seq(0, 1, l = 230) + rnorm(230, sd = .1), 100))

irr_df <- irr_grid %>% 
  mutate(data = f(arg) + rnorm(nrow(irr_grid), sd = .5))
irr_tfd <- tfd(irr_df)
irr_mat <- as.matrix(irr_tfd)
irr_tfb <- tfb_wavelet(irr_tfd, level = 2)

test_that("tfb_wavelet works for irregular grids", {
  expect_is(tfb_wavelet(irr_tfd, verbose = FALSE), "tfb_wavelet")
  expect_equal(length(tfb_wavelet(irr_tfd, verbose = FALSE)), length(irr_tfd))
  for (irr_tfb in list(tfb_wavelet(irr_tfd, verbose = FALSE),
                       tfb_wavelet(irr_mat, verbose = FALSE), 
                       tfb_wavelet(irr_df, verbose = FALSE))) {
    expect_is(irr_tfb, "tfb_wavelet")
    expect_equal(length(irr_tfb), length(irr_tfd))
    expect_equivalent(tf_evaluate(irr_tfb, tf_arg(irr_tfd)), 
                      tf_evaluations(irr_tfb), 
                      tolerance = 1e-1)
  }
})


context("tfb_wavelet glmnet args")

test_that("glmnet arguments work", {
  expect_is(tfb_wavelet(woo_tfd, penalized = TRUE), "tfb_wavelet")
  expect_is(tfb_wavelet(woo_tfd, penalized = TRUE, level = 4), "tfb_wavelet")
  expect_is(tfb_wavelet(woo_tfd, penalized = TRUE, level = 3, filter_number = 1),
            "tfb_wavelet")
  expect_equal(length(tfb_wavelet(woo_tfd,  penalized = TRUE)), 
               length(woo_tfd))
  expect_warning(tfb_wavelet(woo_tfd, penalized = TRUE, level = 5, 
                        filter_number = 4, intercept = FALSE))
  expect_is(tfb_wavelet(woo_tfd, penalized = TRUE, level = 6, filter_number = 1),
            "tfb_wavelet")
  
  expect_is(tfb_wavelet(woo_tfd, penalized = TRUE, type.measure = "mae"),
            "tfb_wavelet")
  expect_is(tfb_wavelet(woo_tfd, penalized = TRUE, nfolds = 5),
            "tfb_wavelet")
  expect_is(tfb_wavelet(woo_tfd, penalized = TRUE, alpha = 0),
            "tfb_wavelet")
})

context("tfb_wavlet works for irregular data")
irr_grid <- data.frame(id = rep(1:100, each = 230), 
                       arg = rep(seq(0, 1, l = 230), 100) + rnorm(230*100, 
                                                                  sd = .1))

irr_df <- irr_grid %>% 
  mutate(data = f(arg) + rnorm(nrow(irr_grid), sd = .5))
irr_tfd <- tfd(irr_df)
irr_mat <- as.matrix(irr_tfd)
irr_tfb <- tfb_wavelet(irr_tfd, level = 2)

test_that("tfb_wavelet works for irregular grids", {
  expect_is(tfb_wavelet(irr_tfd, verbose = FALSE), "tfb_wavelet")
  expect_equal(length(tfb_wavelet(irr_tfd, verbose = FALSE)), length(irr_tfd))
  for (irr_tfb in list(tfb_wavelet(irr_tfd, verbose = FALSE),
                       tfb_wavelet(irr_mat, verbose = FALSE), 
                       tfb_wavelet(irr_df, verbose = FALSE))) {
    expect_is(irr_tfb, "tfb_wavelet")
    expect_equal(length(irr_tfb), length(irr_tfd))
  }
})
