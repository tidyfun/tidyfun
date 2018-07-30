library(devtools)
library(checkmate)
library(dplyr)
library(purrr)
library(scales)
library(ggplot2)

load_all(".")


set.seed(1221)
data = data.frame(col = sample(gl(5, 2)))
data$f = rgp(10); names(data$f) <- letters[1:10]
data$fi = jiggle(data$f)
data$fb = tfb(data$f)

load_all()
gglasagna(data, f, order = col)

tf_eval <- 
  suppressMessages(tf_unnest(data, f, .sep = "___")) %>%
  rename(y = f___id, x = f___arg, fill = f___value)
if (is.null(tf_eval$label)) tf_eval <- mutate(tf_eval, label = y)
ordered <- tf_eval %>%  select(y, label) %>% group_by(y) %>% slice(1)
tf_eval <- tf_eval %>% mutate(y = ordered(y, levels = pull(ordered, y), 
  labels = pull(ordered, label)))
p <- ggplot(tf_eval) + geom_tile(aes(y = y, x = x, fill = fill))
q <- ggplot(data) + geom_lasagna(aes(y  = names(f), fill = f))



ggplot(data, aes(y = f, color = depth(f))) + geom_spaghetti() + 
  annotate("text", x = 1.05, y = drop(data$f[, 1]), label = 1:nrow(data))
ggplot(data, aes(y = f, shape = col, color = col)) + geom_meatballs()
ggplot(data, aes(y = f)) + geom_meatballs(spaghetti = FALSE) + 
  facet_wrap(~col)

# geom_lasagna is a hack of geom_line because geom_tile won't accept the 
# id-factor as a vertical axis -- adjust line width ("size") to get a proper 
# lasagna without gaps just like nonna used to make back in the old country:
load_all(".")
ggplot(data, aes(fill = fb, order = col)) + geom_lasagna()
ggplot(data, aes(tfd = fb)) + geom_lasagna() + facet_wrap(~ col, scales = "free")

# use the order-aesthetic to define the vertical ordering of the functions 
# (low values - low positions)
ggplot(data, aes(tfd = f, order = -depth(f))) + geom_lasagna(size = 8)
# or use order_by to define an ordering computed directly 
# on each function's evaluations:
ggplot(data, aes(tfd = f)) + geom_lasagna(order_by = mean)
ggplot(data, aes(tfd = f)) + geom_lasagna(order_by = min)
ggplot(data, aes(tfd = f)) + geom_lasagna(order_by = function(f) f[1])
# last one same as this:
ggplot(data, aes(tfd = f, order = drop(f[, 0]))) + geom_lasagna()

#can even combine the two: 
ggplot(data, aes(tfd = f, order = col)) + geom_lasagna(order_by = function(f) f[1]) + 
  facet_wrap(col ~ ., scales = "free")

gglasagna(data, fb, order = "col")

# look at GeomSf: https://github.com/tidyverse/ggplot2/blob/17b45f906c3a55d187915f4f3010836a804051ae/R/sf.R

   
   
  
