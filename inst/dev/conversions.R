load <- require(tidyfun)
if (!load) {
  library(devtools)
  load_all(".")
  library(testthat)
  library(checkmate)
  library(dplyr)
  library(purrr)
}

source(system.file("inst/dev/make_examples.R", package = "tidyfun"), echo = TRUE)

################################################################################
# convert

# TODO: empty function / all NAs

as.data.frame(f_reg)
expect_error(as.data.frame(f_reg, arg = seq(-1, 1, l = 10)))
expect_warning(as.data.frame(f_reg, arg = seq(0, 1, l = 31)))
as.data.frame(f_reg, arg = seq(0, 1, l = 31), interpolate = TRUE)

as.data.frame(f_irreg)
as.data.frame(f_irreg, arg = seq(0, 1, l = 11))
as.data.frame(f_irreg, arg = seq(0, 1, l = 11), interpolate = TRUE)

expect_equivalent(as.tfd(as.data.frame(f_reg)), f_reg)
expect_equivalent(as.tfd(as.data.frame(f_irreg)), f_irreg)
expect_equivalent(as.matrix(f_reg), mat_reg)
# NB: this will break if mat_irreg has all-NA columns, which get dropped
# in conversion ... feature or bug ... ?
expect_equivalent(as.matrix(f_irreg), mat_irreg)

list_reg <- as.list(as.data.frame(t(mat_reg))); names(list_reg) <- 1:n
expect_equivalent(tfd(list_reg, arg = grid), f_reg)

list_irreg <- as.list(as.data.frame(t(mat_irreg))); names(list_irreg) <- 1:n
expect_equivalent(tfd(list_irreg, arg = grid), f_irreg)

expect_equivalent(tfd(evaluations(f_irreg), arg = arg(f_irreg)), f_irreg)

list_irreg_mat <- map2(arg(f_irreg), evaluations(f_irreg), ~ cbind(.x, .y))
expect_equivalent(tfd(list_irreg_mat), f_irreg)
list_irreg_df <- map2(arg(f_irreg), evaluations(f_irreg), ~ bind_cols(x=.x, y=.y))
expect_equivalent(tfd(list_irreg_df), f_irreg)

