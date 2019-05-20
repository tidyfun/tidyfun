library(devtools)
library(tidyverse)
library(mgcv)
load_all()


set.seed(17711L)
smoo <- tf_rgp(10, nugget = 0)
rough <- tf_rgp(10, arg = 121L, nugget = .2, scale = .005)
narrow <- tf_jiggle(tf_rgp(10, arg = 11L, nugget = 0))
irr <- tf_sparsify(smoo)


smooexp <- 
  tfb_spline(exp(smoo), family = gaussian(link = "log"), penalized = FALSE)

hetero <- c(
  tf_rgp(10, scale = 0.2, nugget = .05, arg = 101L),
  tf_rgp(10, scale = 0.02, nugget = .05, arg = 101L),
  tf_rgp(10, scale = 0.002, nugget = .05, arg = 101L)) %>%
  tf_sparsify(dropout = .1) %>% tf_jiggle()

#different bases work
layout(t(1:4))
plot(hetero, points = FALSE, col = rep(2:4, e = 10))


# GLM familys work
layout(t(1:4))
hetex <- exp(hetero)
plot(hetex, col = rep(2:4, e = 10), points = FALSE)
plot(tfb_spline(hetex, k = 21, bs= "ps", family = gaussian(link = "log")), 
     col = rep(2:4, e = 10))
plot(tfb_spline(hetex, k = 21, bs= "ps", family = Gamma(link = "log")), 
     col = rep(2:4, e = 10))
plot(tfb_spline(hetex, k = 21, bs= "ps", family = Gamma(link = "inverse")), 
     col = rep(2:4, e = 10), log = "y")

layout(t(1:4))
hetex <- exp(hetero)
plot(hetex, col = rep(2:4, e = 10), points = FALSE, log = "y")
plot(tfb_spline(hetex, k = 21, bs = "ps", family = Gamma(link = "log"),
                global = TRUE), 
     col = rep(2:4, e = 10), log = "y")
plot(tfb_spline(hetex, k = 41, bs = "ps", family = Gamma(link = "log"),
                sp = 1e4), 
     col = rep(2:4, e = 10), log = "y")
plot(tfb_spline(hetex, k = 11, bs = "ps", family = Gamma(link = "log"),
                penalized = FALSE), 
     col = rep(2:4, e = 10), log = "y")


  
