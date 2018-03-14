library(magrittr)
library(tidyverse)

x <- structure(as.list(10:1), class = "weird")
mean.weird <- function(x, na.rm =FALSE) x[[1]]
mean(x)

sum.weird <- function(x, na.rm =FALSE) sum(diff(unlist(x)))
sum(x)

cumsum.weird <- function(x) 
  structure(as.list(rep(x[[1]], length(x))), class = "weird")
cumsum(x)

# hybrid eval DOES not seem to clash with class-specific methods for sum, mean etc:
library(tidyverse)
d <- data_frame(x = x, g = gl(2, 5))
d %>% group_by(g) %>%  summarize(m = mean(x), s = sum(x))
d %>% group_by(g) %>%  mutate(cs = cumsum(x)) %$% cs


# dplyr does not seem to subset by weird
`[.weird` <- function(x, i, j = 2) {
  x <- unlist(x)
  x[i] * j
}

d %>% group_by(g) %>%  mutate(cs = cumsum(x)) %$% cs

#-------------------------------------------------------------------------------

library(devtools)
load_all()

test_df <-  with(refund::DTI, 
  data_frame(id = ID, sex = sex, 
    fi_cca = feval(cca, argvals = seq(0, 1, l = 93)))) %>% 
  mutate(
    f_cca = feval(fi_cca, interpolate = TRUE, argvals = seq(0,1, l = 51)), 
    fb_cca = fbase(f_cca, k = 35))

# slice drops names: don't rely on fvectors being named!
slice <- test_df %>% group_by(sex) %>% slice(n())
slice$fi_cca
slice$fb_cca 
