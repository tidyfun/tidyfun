library(purrr)
library(wavethresh)
library(checkmate)
library(pillar)
library(tidyverse)
library(mgcv)
library(memoise)

set.seed(1234)
data <- data.frame(id = rep(1:5, 8), 
                   arg = rep(1:8, each = 5), 
                   value = rnorm(8*5))

temp <- tfb_wavelet.data.frame(data)
tfb_wavelet.data.frame(data, filter.number = 8, levels = 1, type = "hard",
                       policy = "universal")


tf_evaluate(temp, c(1.5, 2.3, 4))

is_tfb(temp)
temp

devtools::load_all(".")

set.seed(122)
data <- tf_rgp(10, arg = 256, scale = .005)

data_w <- tfb_wavelet.data.frame(as.data.frame(data),
                                 filter.number = 8, levels = 2, type = "hard",
                                 policy = "universal")
str(data_w, 1) # 49 coefs
str(attr(data_w, "basis_matrix"))


set.seed(1234)
data <- data.frame(id = rep(1:5, 8), 
                   arg = rep(1:8, each = 5), 
                   value = rnorm(8*5))

temp <- tfb_wavelet.data.frame(data, level = 1)
tfb_wavelet.data.frame(data, filter.number = 8, levels = 1, type = "hard",
                       policy = "universal")
plot(temp)
lines(tfd(temp), col = 2)
