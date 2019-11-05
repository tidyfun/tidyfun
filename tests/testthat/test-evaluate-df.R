devtools::load_all(".")
context("tf_evaluate for data frames")
d <- tibble(a = tf_rgp(3), b = tf_rgp(3)) 

test_that("tf_evaluate.data.frame basically works", {
  da <- d$a
  expect_identical(tf_evaluate(da)[["a"]],
                   da[["a"]][ , tf_arg(d$a), matrix = FALSE])
})

test_that("tf_evaluate.data.frame interface works", {
  suppressWarnings(rm("a", "b"))
  # avoid weird "Can't join on 'a' x 'a' because of incompatible types (list / list)"
  # error in all.equal.tbl.df (see all.equal(d, d))
  expect_identical(digest::digest(tf_evaluate(d)), 
                    digest::digest(tf_evaluate(d, a, b)))
  expect_identical(tf_evaluate(d, a), 
                   tf_evaluate(d, -b))
  expect_identical(tf_evaluate(d, arg = seq(0, 1, l=11)), 
                   tf_evaluate(d, seq(0, 1, l=11)))
  expect_identical(tf_evaluate(d, b, arg = seq(0, 1, l=11)), 
                   tf_evaluate(d, seq(0, 1, l = 11), b))
  a <- seq(0, 1, l = 11)
  expect_identical(tf_evaluate(d, b, arg = seq(0, 1, l=11)), 
                   tf_evaluate(d, a, b)) #!! a gets interpreted as arg!
})
