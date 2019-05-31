source(system.file("testdata", "make-test-data.R", package = "tidyfun"))

# check constructors from tfd, matrix, data.frame, list
context("tfb_fpc constructor")

test_that("tfb_fpc defaults work for all kinds of regular input", {
  expect_is(tfb_fpc(smoo, verbose = FALSE), "tfb_fpc")
  expect_equal(length(tfb_fpc(smoo, verbose = FALSE)), length(smoo))
  expect_equal(tf_evaluations(tfb_fpc(smoo, verbose = FALSE)), tf_evaluations(smoo),
               tolerance = 1e-3)
  for (smoo_ in list(#tfb_fpc(smoo_list, verbose = FALSE),
                     tfb_fpc(smoo_matrix, verbose = FALSE),
                     tfb_fpc(smoo_df, verbose = FALSE))) {
    expect_is(smoo_, "tfb_spline")
    expect_equal(length(smoo_), length(smoo))
    expect_equivalent(tf_evaluations(smoo_), tf_evaluations(smoo),
                      tolerance = 1e-3)
  }
})
# add missing list method!
# test penalization switch, pve args
