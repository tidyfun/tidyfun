source(system.file("testdata", "make-test-data.R", package = "tidyfun"))

# check constructors from tfd, matrix, data.frame, list
context("tfb_spline constructor: basics")

test_that("tfb_spline defaults work for all kinds of regular input", {
  expect_is(tfb_spline(smoo, verbose = FALSE), "tfb_spline")
  expect_output(tfb_spline(smoo), "100")
  expect_equal(length(tfb_spline(smoo, verbose = FALSE)), length(smoo))
  expect_equivalent(tf_evaluations(tfb_spline(smoo, verbose = FALSE)), tf_evaluations(smoo), 
               tolerance = 1e-3)
  for (dat in list(smoo_list, smoo_matrix, smoo_df)) {
    smoo_ <- try(tfb_spline(dat, verbose = FALSE))
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
                      tolerance = 1e-1)
  }
})

context("tfb_spline constructor: penalization options")

test_that("unpenalized tfb_spline works", {
  expect_error(tfb_spline(narrow, k = 11, penalized = FALSE), "reduce k")
  expect_is(tfb_spline(narrow, k = 8, penalized = FALSE, verbose = FALSE),
            "tfb_spline")
  expect_is(tfb_spline(rough, k = 15, penalized = FALSE, verbose = FALSE),
            "tfb_spline")
  
  expect_is(tfb_spline(exp(smoo), family = Gamma(link = "log"), 
                       penalized = FALSE, verbose = FALSE),
            "tfb_spline")
  expect_is(
    suppressWarnings(
      tfb_spline(narrow^3, family = scat(), k = 5,
                 penalized = FALSE, verbose = FALSE)
    ), "tfb_spline")
  
  expect_equivalent(tfb_spline(irr, k = 11, penalized = FALSE, verbose = FALSE), 
                    tfb_spline(irr, k = 11, verbose = FALSE),
                    tol = 1e-1)
  
  # GLM case: fitting on exp-scale and transforming back:
  expect_equivalent(
    tfb_spline(exp(smoo), family = gaussian(link = "log"), 
               penalized = FALSE, verbose = FALSE) %>% 
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

test_that("mgcv spline basis options work", {
  for (bs in c("tp", "ds", "gp", "ps")) {
    smoo_ <- try(tfb_spline(smoo, k = 21, bs = bs, verbose = FALSE))
    expect_is(smoo_, "tfb_spline")
    expect_equivalent(tf_evaluations(smoo_), tf_evaluations(smoo), 
                      tolerance = 1e-2)
    smoo_spec <- environment(environment(attr(smoo_, "basis"))$`_f`)$spec
    expect_equal(smoo_spec$bs.dim, 21)
    expect_equal(class(smoo_spec), 
                 class(smooth.construct(
                   s(x, bs = bs), data = list(x = 1:40), knots = NULL)))
  }
  smoo_ps <- tfb_spline(smoo, k = 21, bs = "ps", m = c(1,0), verbose = FALSE)
  smoo_spec <- environment(environment(attr(smoo_ps, "basis"))$`_f`)$spec
  smoo_spec$m <- c(1, 0)
})


test_that("global and pre-specified smoothing options work", {
  
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

