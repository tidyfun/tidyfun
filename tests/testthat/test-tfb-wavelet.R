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

is_tfb(temp)
temp
