# deriv
library(devtools)
library(tidyverse)
load_all()

set.seed(12)
grid <- seq(-3.5, 3.5, l = 201)
expr <- f <- 3:-3 * feval(grid, grid) #rgp(3, nugget = 0, argvals = seq(0, 1, l = 101))

plot(fb <- fbase(f, k = 45, bs = "bs"))
plot(fpc <- fpcbase(f, smooth = FALSE))

layout(1:3)
plot(f, col = 1:3)
plot(fd <- deriv(f), col = 1:3)
plot(fdd <- deriv(f, 2), col = 1:3)
f[3, -3:3]
fd[3, -3:3] / (3 * (-3:3)^2)
fdd[3, -3:3] / (6 * -3:3)

plot(fb, col = 1:7)
plot(fbd <- deriv(fb, 1), col = 1:7) 
plot(fbdd <- deriv(fb, 2), col = 1:7) #so sketchy.....

fb[3, -3:3]
fbd[3, -3:3] / (3 * (-3:3)^2)
fbdd[3, -3:3] /  (6 * -3:3)


plot(fpc, col = 1:7)
plot(fpcd <- deriv(fpc, 1), col = 1:7)
plot(fpcdd <- deriv(fpc, 2), col = 1:7)

fpc[3, -3:3]
fpcd[3, -3:3] / (3 * (-3:3)^2)
fpcdd[3, -3:3] /  (6 * -3:3)

#-------------------------------------------------------------------------------

grid <- seq(0, 2, l = 201)
use_f <- function(x) -2 * exp(-x^2) + 2 * cos(2 * x) # x^2 #cos
use_F <- function(x) 2 *exp(-x^2) + sin(2 * x)# 1/2 *x^3#  sin

lg <- length(grid)
f <- feval(use_f(grid), grid)
fb <- fbase(f, basis = "bs", k = 45)
#fpc <- fpcbase(f)

(use_F(grid[lg]) - use_F(grid[1]))
integrate(f)
integrate(tidyfun::as.function.fvector(f), lower = grid[1], upper = grid[lg], 
  subdivisions = 10000)
integrate(use_f, lower = grid[1], upper = grid[lg], 
  subdivisions = 10000)

integrate(tidyfun::as.function.fvector(df[2]), lower = -3.482, upper = 3.482)  
addf <- integrate(deriv(f), definite = FALSE,  lower = -3.482, upper = 3.482)
plot(addf, col = 1:7)
lines(f - drop(f[, -3.5]), col = 2, lty = 2)

df <- deriv(fpc)
(ints <- (-3:3 * 3^3 - (-3:3) * -3^3))
integrate(df, -3.482, 3.482)
integrate(tidyfun::as.function.fvector(df[1]), lower = -3.482, upper = 3.482)
integrate(tidyfun::as.function.fvector(df[2]), lower = -3.482, upper = 3.482)
addf <- integrate(fpcbase(deriv(f)), definite = FALSE, 
  lower = -3.482, upper = 3.482)
plot(addf, col = 1:7, n_grid = -1)
lines(f - drop(f[, -3.5]), col = 2, lty = 2)

set.seed(102)
(f <- rgp(15, nugget = 0, argvals = seq(0, 2, l = 101)))
(df <- deriv(f))
(fi <- integrate(fbase(df), definite = FALSE) + f[,0])
plot(f)
lines(fi, col = 2, lty = 2)


