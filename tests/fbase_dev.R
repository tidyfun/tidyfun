devtools::load_all()
library(testthat)
n <- 5
grid <-  seq(0, 1, l = 11)
mat_reg <- t(replicate(n, 1 + dbeta(grid, runif(1, 2, 7), runif(1, 3, 12))))
colnames(mat_reg) <- grid
rownames(mat_reg) <- 1:n

mat_irreg <- mat_reg
mat_irreg[sample(1:length(mat_reg), length(mat_reg)/3)] <- NA

f_reg <-  feval(mat_reg)
f_irreg <- feval(mat_irreg)

data <- as.data.frame(f_reg)
basis <- "cr"

fb <- new_fbase(data, regular = TRUE,  basis = "ps", k = 10, m = c(2,2))

#
expect_equivalent(attr(fb, "basis")(grid), 
  attr(fb, "basis_matrix"))
