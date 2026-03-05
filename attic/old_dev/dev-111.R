# install.packages("remotes")
remotes::install_github("paleolimbot/ggdebug")
library(tidyverse)
library(ggdebug)
devtools::load_all()
library(tidyfun)

set.seed(1221)
data = data.frame(col = sample(gl(5, 2)))
data$fun = tf_rgp(10)
data$fun_i = tf_jiggle(data$fun)
data$fun_b = tfb(data$fun)
data$fun_p = exp(data$fun)
library(ggplot2)

undebug(ggplot2:::ggplot_build.ggplot)

ggplot(data, aes(y = fun, color = tf_depth(fun))) + geom_spaghetti()

ggplot(data, aes(y = fun, color = tf_depth(fun))) +
  geom_spaghetti() +
  xlim(c(0, 0.5))

ggplot(data, aes(y = fun_i, shape = col, color = col)) + geom_meatballs()

ggplot(data, aes(y = fun_b)) +
  geom_meatballs(spaghetti = FALSE) +
  facet_wrap(~col)


#!! ggplot(data, aes(y = exp(fun_b))) + geom_spaghetti() + scale_y_log10()
ggplot(data, aes(y = fun_p)) + geom_spaghetti() + scale_y_log10()
traceback()
