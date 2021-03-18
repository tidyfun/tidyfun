context("tf_zoom")

set.seed(123)
x <- tf_rgp(4, arg = seq(0, 1, l = 51), nugget = .1)
xi <- tf_sparsify(tf_jiggle(x), .2)
xb <- tfb(x, verbose = FALSE)
xbi <- tfb(xi, verbose = FALSE)

xfpc <- tfb_fpc(x, verbose = FALSE)


test_that("tf_zoom for tfd works", {
  expect_equal(tf_domain(tf_zoom(x, .2, .8)), c(.2, .8))
  expect_equal(tf_domain(tf_zoom(xi, .2, .8)), c(.2, .8))
  expect_equivalent(as.matrix(tf_zoom(x, 0, .5)), as.matrix(x)[, 1:26])
  expect_equivalent(
    as.data.frame(tf_zoom(xi, 0, .5)),
    as.data.frame(xi) %>% dplyr::filter(arg <= .5)
  )

  expect_error(tf_zoom(x, c(.8, .1)))
  expect_error(tf_zoom(x, .11, .111), "no data")
  expect_error(tf_zoom(xi, 0.051, 0.0511), "no data")

  expect_true(is_irreg(tf_zoom(x, .2, seq(.3, 1, l = length(x)))))
})


test_that("tf_zoom for tfb_spline works", {
  expect_equal(tf_domain(tf_zoom(xb, .2, .8)), c(.2, .8))
  expect_equal(tf_domain(tf_zoom(xbi, .2, .8)), c(.2, .8))
  expect_equivalent(
    as.matrix(tf_zoom(xb, 0, .5)), 
    as.matrix(xb)[, 1:26]
  )
  expect_equivalent(
    as.data.frame(tf_zoom(xbi, 0, .5)),
    as.data.frame(xbi) %>% dplyr::filter(arg <= .5)
  )

  expect_error(tf_zoom(xb, c(.8, .1)))
  expect_error(tf_zoom(xb, .11, .111), "no data")

  expect_message(tf_zoom(xb, .2, seq(.3, 1, l = length(x))), "converting to tfd")
  expect_true(is_irreg(tf_zoom(xb, .2, seq(.3, 1, l = length(x)))))
})

test_that("tf_zoom for tfb_fpc works", {
  expect_warning(
    tf_zoom(xfpc, .2, .8), "lose orthogonality of FPC basis")
  expect_equal(
    tf_domain(suppressWarnings(tf_zoom(xfpc, .2, .8))), 
    c(.2, .8)
  )
  expect_equivalent(
    suppressWarnings(as.matrix(tf_zoom(xfpc, 0, .5))), 
    as.matrix(xfpc)[, 1:26]
  )
  expect_equivalent(
    suppressWarnings(as.data.frame(tf_zoom(xfpc, 0, .5))),
    as.data.frame(xfpc) %>% dplyr::filter(arg <= .5)
  )
  
  expect_error(suppressWarnings(tf_zoom(xfpc, .8, .1)))
  expect_error(suppressWarnings(tf_zoom(xfpc, .11, .111)), "no data")
  expect_true(suppressWarnings(
    is_irreg(tf_zoom(xfpc, .2, seq(.3, 1, l = length(x))))))
})
