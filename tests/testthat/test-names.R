context("names and related issues")

data("dti_df")
cca_five <- cca[1:5, seq(0, 1, l = 93), interpolate = TRUE]
cca_five <- tfd(cca_five, signif = 2)
names(cca_five) = LETTERS[1:5]

test_that("names work", {
  expect_equal(names(cca_five), LETTERS[1:5])
  expect_equal(c(mean(cca_five), sd(cca_five)) %>% names, NULL)
  expect_equal(
    c(mean = mean(cca_five), sd = sd(cca_five)) %>% names, 
    c("mean", "sd"))
  expect_equal((1:5 * cca_five[1]) %>% names, NULL)
  expect_equal((3 + cca_five[1]) %>% names, "A")
  expect_error(names(cca_five[1]) <- "")
})
