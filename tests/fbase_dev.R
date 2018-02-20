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
f_irreg2 <- feval(mat_irreg, evaluator = approx_fill_extend)


fb_reg <- new_fbase(as.data.frame(f_reg), regular = TRUE, 
  basis = "ps", k = 10, m = c(2,2), penalized = FALSE)
fb_irreg <- new_fbase(as.data.frame(f_irreg), regular = FALSE, 
  basis = "ps", k = 10, m = c(2,2), penalized = FALSE)
fbp_reg <- new_fbase(as.data.frame(f_reg), regular = TRUE, 
  basis = "ps", k = 30, m = c(2,2), penalized = TRUE)
fbp_irreg <- new_fbase(as.data.frame(f_irreg), regular = FALSE, 
  basis = "ps", k = 30, m = c(2,2), penalized = TRUE)


#
expect_equal(attr(fb_reg, "basis")(grid), 
  attr(fb_reg, "basis_matrix"), tol = 1e-3)
