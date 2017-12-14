source("./tests/make_examples.R", echo = TRUE)

# source("./tests/conversions.R", echo = TRUE)

################################################################################
# sub-indexing: [
# no j-arg --> return subsetted fvector
f_reg[1:2] # same as f_reg[1:2, ] 
f_irreg[-3] 

# with j-arg & raw = FALSE --> return function evaluations in list of tibbles
str(f_reg[1, seq(0, 1, l = 50)])
plot(f_reg[1, seq(0, 1, l = 50)][[1]])
lines(grid, mat_reg[1,], type = "b", col = 2)
str(f_reg[, seq(-.1, .5, l = 6)])

# with j-arg and raw = TRUE --> return function evaluations in matrix
str(f_reg[, seq(0, 1, l = 50), raw = TRUE])
matplot(seq(0, 1, l = 50), t(f_reg[, seq(0, 1, l = 50), raw = TRUE]), 
  col = 1, type = "l", lty = 1)
matlines(grid, t(mat_reg), col = 2, type = "l", lty = 2)

# with I(j)-arg: don't interpolate, only use observed data
str(f_reg[2:3, I(seq(0, 1, l = 21))])
################################################################################
# sub-assignment: [<-]

load_all()
f_reg <- try(feval(mat_reg))
f_reg[2] <- feval(mat_reg[2,, drop = F])
expect_equal(f_reg, feval(mat_reg))
f_reg[-5] <- f_reg[4:1]
expect_equal(names(f_reg), as.character(c(4:1,5)))

f_reg[7] <- f_reg[1]
expect_identical(f_reg[[6]], feval_NA)



