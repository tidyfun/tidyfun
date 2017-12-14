library(devtools)
library(testthat)
library(dplyr)
library(purrr)

load_all(".")

################################################################################
dti <- refund::DTI
f_cca <- feval(dti$cca, argvals = seq(0, 1, l = 93))
test <- data_frame(id = dti$ID, sex = dti$sex, cca = f_cca)

test

test %>% filter(cca[, .7, raw = TRUE] > .6)
test %>% filter(apply(cca[, seq(.7, 1, l = 50), raw = TRUE], 1, max) > .8)
