source(system.file("testdata", "make-test-data.R", package = "tidyfun"))

# check constructors from tfd, matrix, data.frame, list
context("tfb_fpc constructor")

test_that("fpc_wsvd works for smooth equidistant data", {
  expect_is(
    fpc_wsvd(smoo_matrix, attr(smoo_matrix, "arg"), pve = 1.0),
    "list")
  expect_true(
    fpc_wsvd(smoo_matrix, attr(smoo_matrix, "arg"), pve = 0.5)$npc <
      fpc_wsvd(smoo_matrix, attr(smoo_matrix, "arg"), pve = 0.9)$npc)
  fpc_smoo <- 
    fpc_wsvd(smoo_matrix, attr(smoo_matrix, "arg"), pve = 1.0)
  expect_equal(fpc_smoo$npc, length(smoo) - 1)
  expect_equivalent(fpc_smoo$mu, tf_evaluations(mean(smoo))[[1]])
  # check orthonormality for equidistant:
  fpc_smoo <- tfd(t(fpc_smoo$efunctions), arg = attr(smoo_matrix, "arg"))
  expect_true(
    cross2(1:length(fpc_smoo),1:length(fpc_smoo)) %>% 
    map_lgl(~ 
          tf_integrate(fpc_smoo[.x[[1]]] * fpc_smoo[.x[[2]]]) %>% 
          round(digits = 4) %>% 
          {. %in% c(0, 1)}
        ) %>% all)
})

test_that("fpc_wsvd works for smooth non-equidistant data", {
  smoo_arg <- (2 * qbeta(attr(smoo_matrix, "arg"), .5, .8) + 1)^3 
  fpc_smoo <- 
    fpc_wsvd(smoo_matrix, smoo_arg, pve = 1.0)
  expect_equal(fpc_smoo$npc, length(smoo) - 1)
  expect_equivalent(fpc_smoo$mu, tf_evaluations(mean(smoo))[[1]])
  # check orthonormality for non-equidistant:
  fpc_smoo <- tfd(t(fpc_smoo$efunctions), arg = smoo_arg)
  expect_true(
    cross2(1:length(fpc_smoo),1:length(fpc_smoo)) %>% 
      map_lgl(~ 
                tf_integrate(fpc_smoo[.x[[1]]] * fpc_smoo[.x[[2]]]) %>% 
                round(digits = 4) %>% 
                {. %in% c(0, 1)}
      ) %>% all)
})

test_that("tfb_fpc defaults work for all kinds of regular input", {
  expect_is(tfb_fpc(smoo), "tfb_fpc")
  expect_equal(length(tfb_fpc(smoo)), length(smoo))
  expect_equivalent(tf_evaluations(tfb_fpc(smoo + 10)), tf_evaluations(smoo + 10),
               tolerance = 1e-2)
  for (smoo_ in list(#tfb_fpc(smoo_list, verbose = FALSE),
                     tfb_fpc(smoo_matrix),
                     tfb_fpc(smoo_df))) {
    expect_is(smoo_, "tfb_fpc")
    expect_equal(length(smoo_), length(smoo))
    expect_equivalent(tf_evaluations(smoo_), tf_evaluations(smoo),
                      tolerance = 1e-1)
  }
})
# test penalization switch, pve args
