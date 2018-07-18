library(testthat)
library(devtools)
document()
load_all()
n <- 5
grid <-  seq(0, 1, l = 41)
mat_reg <- t(replicate(n, 1 + dbeta(grid, runif(1, 2, 7), runif(1, 3, 12)) + 
    .5 * rnorm(length(grid))))
colnames(mat_reg) <- grid
rownames(mat_reg) <- 1:n

mat_irreg <- mat_reg
mat_irreg[sample(1:length(mat_reg), length(mat_reg)/3)] <- NA

freg <-  tfd(mat_reg, evaluator = approx_spline)
firreg <- tfd(mat_irreg, evaluator = approx_linear)
firreg2 <- tfd(mat_irreg, evaluator = approx_fill_extend)

fbreg <- tfb(as.data.frame(freg))
fbirreg <- tfb(as.matrix(firreg), basis = "tp", k = 10, m = c(2,2), penalized = FALSE)
fbpreg <- tfb(freg, basis = "tp", k = 30, m = c(2,2), penalized = TRUE)
fbpirreg <- tfb(as.matrix(firreg), basis = "tp", k = 30, m = c(2,2), penalized = TRUE)


#
expect_equal(attr(fbreg, "basis")(grid), 
  attr(fbreg, "basis_matrix"), tol = 1e-3)
