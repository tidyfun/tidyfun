library("ggplot2")
set.seed(1221)
data = data.frame(col = sample(gl(5, 2)))
data$f = rgp(10) + 1:10
names(data$f) <- letters[1:10]
data$f <- tfd(data$f)
data$fi = jiggle(data$f)
data$fb = tfb(data$f)

ggplot(data, aes(tf = f, color = depth(f))) + geom_spaghetti() +
  geom_text(aes(x = 1.05, y = drop(f[, 1]), label = names(f)))
ggplot(data, aes(tf = fi, shape = col, color = col)) + geom_meatballs()

ggplot(data, aes(tf = fi)) + geom_meatballs(spaghetti = FALSE) +
  facet_wrap(~col) +
  stat_summary(aes(x = 1.05, y = drop(f[, 1]), color = names(f)), geom = "point")

# geom_lasagna is a hack of geom_line because geom_tile won't accept the
# id-factor as a vertical axis -- adjust line width ("size") to get a proper
# lasagna without gaps just like nonna used to make back in the old country:
ggplot(data, aes(tf = f, order = col)) + geom_lasagna(size = 8)
ggplot(data, aes(tf = fb, order = col)) + geom_lasagna(size = 12)

# use the order-aesthetic to define the vertical ordering of the functions
# (low values - low positions)
ggplot(data, aes(tf = f, order = -depth(f))) + geom_lasagna(size = 8)
# or use order_by to define an ordering computed directly
# on each function's evaluations:
ggplot(data, aes(tf = f)) + geom_lasagna(order_by = mean)
ggplot(data, aes(tf = f)) + geom_lasagna(order_by = min)
ggplot(data, aes(tf = f)) + geom_lasagna(order_by = function(f) f[1])
# last one same as this:
ggplot(data, aes(tf = f, order = drop(f[, 0]))) + geom_lasagna()
# combine the two:
ggplot(data, aes(tf = f, order = col)) + geom_lasagna()
# .. but facetting is broken for lasagna:
ggplot(data, aes(tf = f, y = names(f))) +
  geom_lasagna()  + facet_wrap(~ col)

# TODO:  this does not do anything sensible
data$idf <- paste0(names(data$f),"_f")
ggplot(data) + geom_lasagna(aes(tf = f, y = idf))


