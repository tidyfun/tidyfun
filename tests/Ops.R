library(testthat)
library(devtools)
document()
load_all()
n <- 5
grid <-  seq(0, 1, l = 31)
mat_reg <- t(replicate(n, 1 + dbeta(grid, runif(1, 2, 7), runif(1, 3, 12)) + 
    .5 * rnorm(length(grid))))
colnames(mat_reg) <- grid
rownames(mat_reg) <- 1:n

mat_irreg <- mat_reg
mat_irreg[sample(1:length(mat_reg), length(mat_reg)/3)] <- NA

f_reg <-  feval(mat_reg, evaluator = approx_spline)
f_irreg <- feval(mat_irreg, evaluator = approx_linear)
fbp <- fbase(f_reg, k = 15)

debugonce(Ops.fvector)
+fbp

debugonce(Ops.fvector)
-fbp

