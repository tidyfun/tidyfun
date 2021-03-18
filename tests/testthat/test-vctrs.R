context("vctrs")

x = tf_rgp(1, arg = seq(0, 1, l = 11))
y = tf_rgp(1, arg = seq(0, 1, l = 21))

cca = dti_df$cca
rcst = dti_df$cca

test_that("concatenation behaves for tfd", {
  expect_warning(c(x, y), "different grids")
  expect_warning(c(x, y), "different resolutions")
  expect_is(suppressWarnings(c(x, y)), "tfd_irreg")
  expect_is(c(cca[1], rcst[1]), "tfd_irreg")
  expect_is(c(x, x), "tfd_reg")
  expect_error(c(x, tfb(x)))
  expect_numeric(suppressWarnings(tf_evaluate(c(x, y)[1])[[1]]))
})


tfb_k10 = cca %>% tfb(k = 10)
tfb_k20 = cca %>% tfb(k = 20)
z = tf_rgp(10, arg = seq(0, 1, l = 11)) %>% tfb_fpc()
z2 = z %>% tfb_fpc(npc = 10)

test_that("concatenation behaves for tfb", {
  expect_is(c(tfb_k10[1], tfb_k10[2]), "tfb_spline")
  expect_is(c(tfb(tfb_k10[1]), tfb(tfb_k10[2])), "tfb_spline")
  expect_is(c(z, z), "tfb_fpc")
  
  expect_warning(c(tfb_k10, tfb_k20))
  expect_error(c(tfb_k10, tfb_fpc(z)))
  expect_error(c(tfb_fpc(z), tfb_k10))
  expect_error(c(z, z2))
})
