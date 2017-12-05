library(devtools)
library(testthat)
library(dplyr)
library(purrr)

load_all(".")


n <- 5
grid <-  seq(0, 1, l = 11)
mat_reg <- t(replicate(n, dbeta(grid, runif(1, 2, 7), runif(1, 3, 12))))
colnames(mat_reg) <- grid
mat_irreg <- mat_reg
mat_irreg[sample(1:length(mat_reg), length(mat_reg)/3)] <- NA

################################################################################
# construct / convert

f_reg <- try(feval(mat_reg))
expect_true(all(sapply(f_reg, is.function)))
expect_true(length(f_reg) == n)

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
f_irreg

################################################################################
# [, [[, 
# no j-arg --> return subsetted fvector
f_reg[1:2] # same as f_reg[1:2, ] 
f_irreg[-3] 

# with j-arg & raw = FALSE --> return function evaluations in list of tibbles
str(f_reg[1, seq(0, 1, l = 50)])
str(f_reg[2:3, seq(0, 1, l = 50), interpolate = FALSE])
str(f_reg[, seq(-.1, .5, l = 6)])

# with j-arg and raw = TRUE --> return function evaluations in matrix
str(f_reg[, seq(0, 1, l = 50), raw = TRUE])
str(f_reg[1, seq(0, 1, l = 50), raw = TRUE, interpolate = FALSE])

################################################################################
# in a tibble
dti <- refund::DTI
f_cca <- feval(dti$cca, argvals = seq(0, 1, l = 93))
test <- data_frame(id = dti$ID, sex = dti$sex, cca = f_cca)

test

test %>% filter(cca[, .7, raw = TRUE] > .6)
test %>% filter(rowMeans(cca[, seq(.7, 1, l = 10), raw = TRUE]) > .6)

