library(testthat)
library(devtools)
document()
load_all()

fe <- rgp(5, grid = seq(0, 1, l = 21))
plot(fe, p = FALSE)
lines(cummax(fe), col = 1:5)
fm <- tfd(apply(as.matrix(fe), 2, cummax))
plot(fm, col = 1:5, lty = 2)

set.seed(2211)
n <- 5
grid <-  seq(0, 1, l = 31)
mat_reg <- t(replicate(n, 1 + dbeta(grid, runif(1, 2, 7), runif(1, 3, 12)) + 
    .5 * rnorm(length(grid))))
colnames(mat_reg) <- grid
rownames(mat_reg) <- 1:n

mat_irreg <- mat_reg
mat_irreg[sample(1:length(mat_reg), length(mat_reg)/3)] <- NA

freg <-  tfd(mat_reg, evaluator = approx_spline)
firreg <- tfd(mat_irreg, evaluator = approx_linear)


freg == +freg
freg - 2 * freg == -freg
freg + freg == freg * 2
freg^2 == freg * freg
2^freg
freg == freg[2]

firreg == +firreg
firreg - 2 * firreg == -firreg
firreg + firreg == firreg * 2
firreg^2 == firreg * firreg
2^firreg
firreg == firreg[2]


fb <- tfb(freg, k = 25)
# NB:
all.equal(coef(tfb(tfd(tfb(freg)))), coef(tfb(freg)))
tfb(tfd(tfb(freg))) == tfb(freg)
# but:
purrr::map2(coef(tfb(tfd(tfb(freg)))), coef(tfb(freg)), 
  ~all(.x == .y))

+fb == fb
-fb == fb - 2*fb

fb * fb == fb^2
2^fb

all.equal(log(exp(fb)), fb, tol = 1e-3)
all.equal(sinh(sin(tfb(seq(0,1,l = 20)))), 
  tfb(seq(0,1,l = 20)), tol = 1e-3)

f1 <- tfb(dbeta(seq(0, 1, l = 21), 7, 3), argvals = seq(0, 1, l = 21))
f2 <- tfb(dbeta(seq(0, 1, l = 13), 2, 3), argvals = seq(0, 1, l = 13), 
  domain = c(0,1))
f3 <- c(f1, f2)  
f4 <- c(f2, f1) 

data_irreg <- data.frame(id = rep(1:3, each = 21), 
  argvals = round(rep(seq(0, 1, l = 21), times = 3) + runif(63, -.1, .1), 4),
  data = dbeta(rep(seq(0, 1, l = 21), times = 3), 3, 7))  #%>% 
  #group_by(id) %>%  arrange(argvals, .by_group = TRUE) %>%  ungroup()
firreg <- tfd(data_irreg)

data_reg <- data.frame(id = rep(1:3, each = 21), 
  argvals = rep(seq(0, 1, l = 21), times = 3),
  data = as.vector(replicate(3, dbeta(seq(0, 1, l = 21), 
    runif(1, 2, 5), runif(1, 3, 9)))))
freg <- tfd(data_reg)
range(freg)

fb <- tfb(firreg, k = 10)
fb2 <- tfb(data_irreg, k = 10)

range(fb)
plot(fb)
lines(range(fb), col = 2) 

fb <- tfb(data_irreg, k = 10)
