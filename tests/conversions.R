source("./tests/make_examples.R", echo = TRUE)

################################################################################
# convert

# TODO: empty function / all NAs

as.data.frame(f_reg)
expect_error(as.data.frame(f_reg, argvals = seq(-1, 1, l = 10)))
expect_warning(as.data.frame(f_reg, argvals = seq(0, 1, l = 31)))
as.data.frame(f_reg, argvals = seq(0, 1, l = 31), interpolate = TRUE)

as.data.frame(f_irreg)
as.data.frame(f_irreg, argvals = seq(0, 1, l = 11))
as.data.frame(f_irreg, argvals = seq(0, 1, l = 11), interpolate = TRUE)

expect_equivalent(as.feval(as.data.frame(f_reg)), f_reg)
expect_equivalent(as.feval(as.data.frame(f_irreg)), f_irreg)
expect_equivalent(as.matrix(f_reg), mat_reg)
# NB: this will break if mat_irreg has all-NA columns, which get dropped
# in conversion ... feature or bug ... ?
expect_equivalent(as.matrix(f_irreg), mat_irreg)

list_reg <- as.list(as.data.frame(t(mat_reg))); names(list_reg) <- 1:n
expect_equivalent(feval(list_reg, argvals = grid), f_reg)

list_irreg <- as.list(as.data.frame(t(mat_irreg))); names(list_irreg) <- 1:n
expect_equivalent(feval(list_irreg, argvals = grid), f_irreg)

expect_equivalent(feval(evaluations(f_irreg), argvals = argvals(f_irreg)), f_irreg)

list_irreg_mat <- map2(argvals(f_irreg), evaluations(f_irreg), ~ cbind(.x, .y))
expect_equivalent(feval(list_irreg_mat), f_irreg)
list_irreg_df <- map2(argvals(f_irreg), evaluations(f_irreg), ~ bind_cols(x=.x, y=.y))
expect_equivalent(feval(list_irreg_df), f_irreg)

################################################################################
# print

f_reg
print(f_reg, digits = 3)
print(f_reg, digits = 2, show = 11)

f_irreg
print(f_irreg, digits = 2, n= 2, show = 6)
