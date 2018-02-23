library(testthat)
library(devtools)
document()
load_all()

set.seed(2211)
n <- 5
grid <-  seq(0, 1, l = 31)
mat_reg <- t(replicate(n, 1 + dbeta(grid, runif(1, 2, 7), runif(1, 3, 12)) + 
    .5 * rnorm(length(grid))))
colnames(mat_reg) <- grid
rownames(mat_reg) <- 1:n

mat_irreg <- mat_reg
mat_irreg[sample(1:length(mat_reg), length(mat_reg)/3)] <- NA

freg <-  feval(mat_reg, evaluator = approx_spline)
firreg <- feval(mat_irreg, evaluator = approx_linear)

freg == +freg
freg - 2 * freg == -freg
freg + freg == freg * 2
freg^2 == freg * freg
2^freg
freg == freg[2]

fb <- fbase(freg, k = 25)
# NB:
all.equal(coef(fbase(feval(fbase(freg)))), coef(fbase(freg)))
fbase(feval(fbase(freg))) == fbase(freg)
# but:
purrr::map2(coef(fbase(feval(fbase(freg)))), coef(fbase(freg)), 
  ~all(.x == .y))

+fb == fb
-fb == fb - 2*fb

fb * fb == fb^2
2^fb

log(exp(fb)) == fb
