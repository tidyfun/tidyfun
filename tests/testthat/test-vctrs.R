context("vctrs")

x = tf_rgp(1, arg = seq(0, 1, l = 11))
y = tf_rgp(1, arg = seq(0, 1, l = 21))

cca = dti_df$cca
cca_tfb = cca %>% tfb()

rcst = dti_df$cca

test_that("concatenation behaves", {
  expect_is(c(x, y), "tfd_irreg")
  expect_is(c(cca[1], rcst[1]), "tfd_irreg")
  expect_is(c(x, x), "tfd_reg")
  expect_is(c(cca_tfb[1], cca_tfb[2]), "tfb")
  expect_error(c(x, tfb(x)))
  
  expect_numeric(tf_evaluate(c(x, y)[1])[[1]])
})
