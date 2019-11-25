library(purrr)
library(wavethresh)
library(checkmate)
library(pillar)
library(tidyverse)
library(mgcv)
library(memoise)

set.seed(1234)
data <- data.frame(id = rep(1:5, 8), 
                   arg = rep(1:8, each = 5), 
                   value = rnorm(8*5))

temp <- tfb_wavelet.data.frame(data)
tfb_wavelet.data.frame(data, filter.number = 8, levels = 1, type = "hard",
                       policy = "universal")


tf_evaluate(temp, c(1.5, 2.3, 4))

is_tfb(temp)
temp

devtools::load_all(".")


set.seed(1234)
data <- data.frame(id = rep(1:5, 8), 
                   arg = rep(1:8, each = 5), 
                   value = rnorm(8*5))

temp <- tfb_wavelet.data.frame(data, level = 2, filter_number = 3)
tfb_wavelet.data.frame(data, filter.number = 8, levels = 1, type = "hard",
                       policy = "universal")
plot(temp)
lines(tfd(temp), col = 2)



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
  for (dat in list(woo_df, woo_tfd)) {
    woo_ <- try(tfb_wavelet(dat, verbose = FALSE))
    expect_is(woo_, "tfb_wavelet")
    expect_equal(length(woo_), length(woo_tfd))
    expect_equivalent(tf_evaluations(woo_), tf_evaluations(woo_tfd),
                      tolerance = 1)
  }
})


test_that("tfb_wavelet works for different wavelet parameters", {
  for (i in 1:10) {
    for (j in 2:10) {
      tfb_wavelet(woo_tfd, level = j, filter_number = i) 
      print(paste0(i,".", j))
    }
  }
})

test_that("tfb_wavelet independent of datatype", {
  expect_equal(tfb_wavelet(woo_tfd), tfb_wavelet(woo_df))
  expect_equal(tfb_wavelet(woo_tfd), tfb_wavelet(woo_tfb))
  expect_equal(tfb_wavelet(woo_tfd), tfb_wavelet(woo_mat))
})


woo_32768 <- data.frame(id = rep(1:100, each = 32768), 
                        arg = rep(seq(0, 1, l = 32768), 100))

woo_32786_df <- woo_32768 %>% mutate(data = f(arg) + 
                                       rnorm(nrow(woo_32768), sd = .5))

bench::mark(
  data_256 = tfb_wavelet(woo_df), # .5s and 13.75MP
  data_32768 = tfb_wavelet(woo_32786_df), # 1.9s and 1GB
  data_32768 = tfb_wavelet(woo_32786_df, level = 6), # 3s and 2.77GB
  spline_32768 = tfb_spline(woo_32786_df), # 6s and 2.63GB
  check = FALSE
)

library(profvis)
profvis(tfb_wavelet(woo_32786_df, level = 6)) 


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
  expect_is(tfb_wavelet(woo_tfd, penalized = FALSE), "tfb_wavelet")
  expect_equal(length(tfb_wavelet(woo_tfd, penalized = FALSE)), 
               length(woo_tfd))
  expect_equivalent(tfb_wavelet(woo_tfd, penalized = FALSE),
                    tfb_wavelet(woo_tfd, penalized = FALSE, nlambda = 20),
                    tolerance = 1e-1)
})

test_that("mgcv spline basis options work", {
  for (bs in c("tp", "ds", "gp", "ps")) {
    woo_ <- try(tfb_wavelet(woo, k = 21, bs = bs, verbose = FALSE))
    expect_is(woo_, "tfb_wavelet")
    expect_equivalent(tf_evaluations(woo_), tf_evaluations(woo), 
                      tolerance = 1e-2)
    woo_spec <- environment(environment(attr(woo_, "basis"))$`_f`)$spec
    expect_equal(woo_spec$bs.dim, 21)
    expect_equal(class(woo_spec), 
                 class(wooth.construct(
                   s(x, bs = bs), data = list(x = 1:40), knots = NULL)))
  }
  woo_ps <- tfb_wavelet(woo, k = 21, bs = "ps", m = c(1,0), verbose = FALSE)
  woo_spec <- environment(environment(attr(woo_ps, "basis"))$`_f`)$spec
  woo_spec$m <- c(1, 0)
})


test_that("global and pre-specified woothing options work", {
  
  rough_global <- try(tfb(rough, global = TRUE, k = 51, verbose = FALSE))
  expect_is(rough_global, "tfb")
  expect_true(
    system.time(
      tfb(c(rough, rough, rough), k = 51, verbose = FALSE)
    )["elapsed"] > 
      system.time(
        tfb(c(rough, rough, rough), k = 51, global = TRUE, verbose = FALSE)
      )["elapsed"] 
  )
  
  expect_equivalent(
    tfb(rough, sp = 1e-15, k = 51, verbose = FALSE) %>% tf_evaluations,
    tfb(rough, penalized = FALSE, k = 51, verbose = FALSE) %>% tf_evaluations)
  expect_equivalent(
    tfb(rough, sp = .2, k = 75, verbose = FALSE) %>% tf_evaluations,
    tfb(rough, sp = .2, k = 10, verbose = FALSE) %>% tf_evaluations, 
    tol = 1e-2)
  
  expect_equivalent(
    tfb(exp(rough), sp = 1e-15, k = 51, family = gaussian(link = "log"),
        verbose = FALSE) %>%
      tf_evaluations,
    tfb(exp(rough), penalized = FALSE, k = 51, family = gaussian(link = "log"),
        verbose = FALSE) %>%
      tf_evaluations, 
    tol = 1e-3)
  expect_equivalent(
    tfb(exp(rough), sp = .2, k = 75, family = gaussian(link = "log"), 
        verbose = FALSE) %>% tf_evaluations,
    tfb(exp(rough), sp = .2, k = 10, family = gaussian(link = "log"), 
        verbose = FALSE) %>% tf_evaluations, 
    tol = 1e-2)
})

