library(devtools)
library(testthat)
library(dplyr)
library(purrr)

load_all(".")

################################################################################
n <- 5
grid <-  seq(0, 1, l = 11)
mat_reg <- t(replicate(n, dbeta(grid, runif(1, 2, 7), runif(1, 3, 12))))
colnames(mat_reg) <- grid
rownames(mat_reg) <- 1:n

mat_irreg <- mat_reg
mat_irreg[sample(1:length(mat_reg), length(mat_reg)/3)] <- NA
