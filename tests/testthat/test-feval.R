library(testthat)

n <- 5
grid <-  seq(0, 1, l = 11)
mat_reg <- t(replicate(n, dbeta(grid, runif(1, 2, 7), runif(1, 3, 12))))
colnames(mat_reg) <- grid
mat_irreg <- mat_reg; mat_irreg[sample(1:length(mat_reg), length(mat_reg)/3)] <- NA

f_reg <- feval(mat_reg)
str(f_reg, 1)
str(argvals(f_reg), 1)

f_irreg <- feval(mat_irreg)
str(f_irreg, 1)
str(argvals(f_irreg), 1)

as.data.frame(f_reg)
as.data.frame(f_irreg)

expect_equivalent(as.feval(as.data.frame(f_reg)), f_reg)
expect_equivalent(as.feval(as.data.frame(f_irreg)), f_irreg)
expect_equivalent(as.matrix(f_reg), mat_reg)
expect_equivalent(as.matrix(f_irreg), mat_irreg)
