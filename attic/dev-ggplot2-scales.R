library(tidyverse)
devtools::load_all(".")

d <- tibble(f = exp(tf_rgp(3)))

ggplot(d) + geom_spaghetti(aes(y = f))
ggplot(d) + geom_spaghetti(aes(y = f)) + scale_y_log10()
ggplot(d) + geom_spaghetti(aes(tf = f)) + scale_y_log10()
