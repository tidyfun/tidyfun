context("zooming")

set.seed(123)
x <- rgp(4, arg = seq(0, 1, l = 51), nugget = .1)
xi <- sparsify(jiggle(x), .2)
xb <- tfb(x, verbose = FALSE)
xbi <- tfb_fpc(xi)


test_that("zoom for tfd works", {
  expect_equal(domain(zoom(x, .2, .8)), c(.2, .8))
  expect_equal(domain(zoom(xi, .2, .8)), c(.2, .8))
  expect_equivalent(as.matrix(zoom(x, 0, .5)), as.matrix(x)[,1:26])
  expect_equivalent(as.data.frame(zoom(xi, 0, .5)), 
    as.data.frame(xi) %>% filter(arg <= .5))
  
  expect_error(zoom(x, c(.8, .1)))
  expect_error(zoom(x, .11, .111), "no data")
  expect_error(zoom(xi, 0.051, 0.0511), "no data")
  
  expect_true(is_irreg(zoom(x, .2, seq(.3, 1, l = length(x)))))
})


test_that("zoom for tfb works", {
  expect_equal(domain(zoom(xb, .2, .8)), c(.2, .8))
  expect_equal(domain(zoom(xbi, .2, .8)), c(.2, .8))
  expect_equivalent(as.matrix(zoom(xb, 0, .5)), as.matrix(xb)[,1:26])
  expect_equivalent(as.data.frame(zoom(xbi, 0, .5)), 
    as.data.frame(xbi) %>% filter(arg <= .5))
  
  expect_error(zoom(xb, c(.8, .1)))
  expect_error(zoom(xb, .11, .111), "no data")

  expect_message(zoom(xb, .2, seq(.3, 1, l = length(x))), "converting")
  expect_true(is_irreg(zoom(xb, .2, seq(.3, 1, l = length(x)))))
})
