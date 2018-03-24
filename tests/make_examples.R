n <- 5
grid <-  seq(0, 1, l = 11)
mat_reg <- t(replicate(n, 1 + dbeta(grid, runif(1, 2, 7), runif(1, 3, 12))))
colnames(mat_reg) <- grid
rownames(mat_reg) <- n:1

mat_irreg <- mat_reg
mat_irreg[sample(1:length(mat_reg), length(mat_reg)/3)] <- NA

f_reg <- try(feval(mat_reg))
expect_true(length(f_reg) == n)

str(argvals(f_reg), 1)
domain(f_reg)
evaluator(f_reg)

f_irreg <- feval(mat_irreg)
str(f_irreg, 1)
str(argvals(f_irreg), 1)

