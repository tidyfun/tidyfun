load <- require(tidyfun)
if (!load) {
  library(devtools)
  load_all(".")
  library(testthat)
  library(checkmate)
  library(dplyr)
  library(purrr)
}  

source(system.file("tests/make_examples.R", package = "tidyfun"), echo =TRUE)

################################################################################
# sub-indexing: [

# no j-arg --> return subsetted fvector
f_reg[1:2] # same as f_reg[1:2, ] 
f_reg[1:2, ] 
f_irreg[-3] 
f_reg["5"]
f_reg[c(T,F,T,T,F)]

new_grid <- seq(0, 1, l = 50)

# with j-arg and matrix = TRUE (default)  --> return function evaluations in matrix
str(f_reg[, new_grid])
matplot(new_grid, t(f_reg[, new_grid]), 
  col = 1, type = "l", lty = 1)
matlines(grid, t(mat_reg), col = 2, type = "l", lty = 2)

matplot(new_grid, t(f_irreg[, new_grid]), 
  col = 1, type = "l", lty = 1)
matlines(grid, t(mat_irreg), col = 2, type = "b", lty = 2, pch="x")


checkmate::expect_array(f_reg[, new_grid], d = 2)
expect_equal(nrow(f_reg[, new_grid]), length(f_reg))
expect_equal(ncol(f_reg[, new_grid]), length(new_grid))
expect_equal(row.names(f_reg[, new_grid]), names(f_reg)) 
expect_equal(colnames(f_reg[, new_grid]), 
  as.character(adjust_resolution(new_grid, f_reg)))

# with j-arg & matrix = FALSE --> 
# return (interpolated) function evaluations in named list of tibbles

plot(f_reg[1, new_grid, matrix = FALSE][[1]])
lines(grid, mat_reg[1,], type = "b", col = 2)

expect_error((f_reg[, seq(-.1, .5, l = 6), matrix = FALSE]), ">= 0")
expect_error((f_reg[, seq(0, 1.5, l = 6), matrix = FALSE]), "<= 1")
checkmate::expect_data_frame(f_reg[1, argvals(f_reg), matrix = FALSE][[1]])
expect_true(length(f_reg[, argvals(f_reg), matrix = FALSE]) == length(f_reg))
expect_equal(names(f_reg[, argvals(f_reg), matrix = FALSE]), names(f_reg))



# with j-arg and interpolate = FALSE: return NA for argvals not in the original data
expect_warning(f_reg[2:3, seq(0, 1, l = 21), interpolate = FALSE], "no evaluations")
expect_identical(f_reg[1, argvals(f_reg)[1:5], interpolate = FALSE], 
  f_reg[1, argvals(f_reg)[1:5]])
expect_true(all(is.na(f_reg[2:3, 0.123, interpolate = FALSE, matrix = TRUE])))

################################################################################

# sub-assignment: [<-]
f_reg[3] <- feval(mat_reg[2,, drop = F])
f_irreg[-5] <- f_irreg[4:1]
expect_equivalent(f_reg[1:3], feval(mat_reg[c(1,2,2),]))
expect_equal(names(f_irreg), as.character(c(2:5,1)))


#check <NA>-functions:
f_reg[7] <- f_reg[1]
# f_reg[6] is a "functional missing value"
expect_identical(f_reg[[6]], rep(1*NA, n_evaluations(f_reg)))
checkmate::expect_scalar_na(unique(f_reg[6, argvals(f_reg)][1,]))

f_irreg[7] <- f_irreg[1]
checkmate::expect_scalar_na(unique(f_irreg[6, argvals(f_reg)][1,]))

