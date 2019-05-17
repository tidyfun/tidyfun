set.seed(17711L)
smoo <- tf_rgp(10, nugget = 0)
rough <- tf_rgp(10, arg = 121L, nugget = .2, scale = .005)
narrow <- tf_jiggle(tf_rgp(10, arg = 11L, nugget = 0))
irr <- tf_sparsify(smoo)

smoo_list <- tf_evaluations(smoo)
smoo_matrix <- as.matrix(smoo)
smoo_df <- as.data.frame(smoo)
irr_list <- tf_evaluations(irr)
irr_matrix <- suppressWarnings(as.matrix(irr))
irr_df <- as.data.frame(irr)
narrow_df <- as.data.frame(narrow) 

#layout(t(1:3)); plot(smoo); plot(rough); plot(narrow)

# check constructors from tfd, matrix, data.frame, list
context("tfb_spline constructor")

test_that("tfb_spline defaults work for all kinds of regular input", {
  expect_is(tfb_spline(smoo, verbose = FALSE), "tfb_spline")
  expect_output(tfb_spline(smoo), "100")
  expect_equal(length(tfb_spline(smoo, verbose = FALSE)), length(smoo))
  expect_equal(tf_evaluations(tfb_spline(smoo, verbose = FALSE)), tf_evaluations(smoo), 
               tolerance = 1e-3)
  for (smoo_ in list(tfb_spline(smoo_list, verbose = FALSE), 
                     tfb_spline(smoo_matrix, verbose = FALSE), 
                     tfb_spline(smoo_df, verbose = FALSE))) {
    expect_is(smoo_, "tfb_spline")
    expect_equal(length(smoo_), length(smoo))
    expect_equivalent(tf_evaluations(smoo_), tf_evaluations(smoo), 
                 tolerance = 1e-3)
  }
})

test_that("tfb_spline defaults work for all kinds of irregular input", {
  expect_is(tfb_spline(irr, verbose = FALSE), "tfb_spline")
  expect_output(tfb_spline(irr), "100")
  expect_equal(length(tfb_spline(irr, verbose = FALSE)), length(irr))
  expect_output(tfb_spline(irr_df), "100")
  for (irr_tfb in list(tfb_spline(irr_list, arg = tf_arg(irr), verbose = FALSE),
                      tfb_spline(irr_matrix, verbose = FALSE), 
                      tfb_spline(irr_df, verbose = FALSE))) {
    expect_is(irr_tfb, "tfb_spline")
    expect_equal(length(irr_tfb), length(irr))
    expect_equivalent(tf_evaluate(irr_tfb, tf_arg(irr)), 
                      tf_evaluations(irr), 
                      tolerance = 1e-3)
  }
})

test_that("unpenalized tfb_spline works", {
  expect_error(tfb_spline(narrow, k = 11, penalized = FALSE), "reduce k")
  expect_is(tfb_spline(narrow, k = 8, penalized = FALSE, verbose = FALSE),
            "tfb_spline")
  expect_is(tfb_spline(rough, k = 15, penalized = FALSE, verbose = FALSE),
            "tfb_spline")
  expect_is(tfb_spline(exp(smoo), family = Gamma(link = "log"), 
                       penalized = FALSE, verbose = FALSE),
            "tfb_spline")
  expect_is(tfb_spline(rough^3, family = scat(), 
                       penalized = FALSE, verbose = FALSE),
            "tfb_spline")
  expect_equivalent(tfb_spline(irr, k = 11, penalized = FALSE), 
                    tfb_spline(irr, k = 11),
                    tol = 1e-2)
  
  # GLM case: fitting on exp-scale and transforming back:
  expect_equivalent(
    tfb_spline(exp(smoo), family = gaussian(link = "log"), penalized = FALSE) %>% 
      log %>% as.matrix, 
    as.matrix(smoo), 
    tolerance = .001)
  
  expect_message(
    try(tfb_spline(smoo, family = Gamma(link = "log"), penalized = FALSE),
        silent = TRUE),
    "non-positive")
  expect_error(
    suppressMessages(
      tfb_spline(smoo, family = Gamma(link = "log"), penalized = FALSE)
    ),
    "Basis representation failed")
  
  approx_penalized <- abs(rough - tfd(tfb(rough, k = 40, verbose = FALSE))) %>% 
     as.matrix %>% sum
  approx_unpenalized <- abs(rough - tfd(tfb(rough, k = 40, penalized = FALSE, 
                                            verbose = FALSE))) %>% 
    as.matrix %>% sum
  expect_true(approx_penalized > approx_unpenalized)
})

test_that("global smoothing option works", {
 ##
})

# local - global - prespecified
# ls - glm
# regular - irregular

tfb_spline(smoo, family = Gamma(link = "log"), penalized = FALSE)

ruff <- 
  (rough + seq(-5, 5, l = 10)) + seq(-2, 2, l = 10) * tfd(tf_arg(rough), tf_arg(rough))

tfb(ruff, global = TRUE)

# # check constructors from tfd, matrix, data.frame, list
# context("tfb_fpc constructor")
# 
# test_that("tfb_fpc defaults work for all kinds of regular input", {
#   expect_is(tfb_fpc(smoo, verbose = FALSE), "tfb_spline")
#   expect_output(tfb_spline(smoo), "100")
#   expect_equal(length(tfb_spline(smoo, verbose = FALSE)), length(smoo))
#   expect_equal(tf_evaluations(tfb_spline(smoo, verbose = FALSE)), tf_evaluations(smoo), 
#                tolerance = 1e-3)
#   for (smoo_ in list(tfb_spline(smoo_list, verbose = FALSE), 
#                      tfb_spline(smoo_matrix, verbose = FALSE), 
#                      tfb_spline(smoo_df, verbose = FALSE))) {
#     expect_is(smoo_, "tfb_spline")
#     expect_equal(length(smoo_), length(smoo))
#     expect_equivalent(tf_evaluations(smoo_), tf_evaluations(smoo), 
#                       tolerance = 1e-3)
#   }
# })
# 
# test_that("tfb_spline defaults work for all kinds of irregular input", {
#   expect_is(tfb_spline(irr, verbose = FALSE), "tfb_spline")
#   expect_output(tfb_spline(irr), "100")
#   expect_equal(length(tfb_spline(irr, verbose = FALSE)), length(irr))
#   expect_output(tfb_spline(irr_df), "100")
#   for (irr_tfb in list(tfb_spline(irr_list, arg = tf_arg(irr), verbose = FALSE),
#                        tfb_spline(irr_matrix, verbose = FALSE), 
#                        tfb_spline(irr_df, verbose = FALSE))) {
#     expect_is(irr_tfb, "tfb_spline")
#     expect_equal(length(irr_tfb), length(irr))
#     expect_equivalent(tf_evaluate(irr_tfb, tf_arg(irr)), 
#                       tf_evaluations(irr), 
#                       tolerance = 1e-3)
#   }
# })
# 
# test_that("tfb_spline penalization switch works", {
#   expect_error(tfb_spline(narrow, k = 11, penalized = FALSE), "reduce k")
#   expect_is(tfb_spline(narrow, k = 8, penalized = FALSE, verbose = FALSE),
#             "tfb_spline")
#   expect_is(tfb_spline(rough, k = 15, penalized = FALSE, verbose = FALSE),
#             "tfb_spline")
#   plot(tfb(rough, k = 100, verbose = FALSE))
#   plot(tfb(rough, k = 100, penalized = FALSE))
#   approx_penalized <- abs(rough - tfd(tfb(rough, k = 40, verbose = FALSE))) %>% 
#     as.matrix %>% sum
#   approx_unpenalized <- abs(rough - tfd(tfb(rough, k = 40, penalized = FALSE, 
#                                             verbose = FALSE))) %>% 
#     as.matrix %>% sum
#   expect_true(approx_penalized > approx_unpenalized)
# })
