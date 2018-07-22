library("ggplot2")
set.seed(1221)
data = data.frame(col = sample(gl(5, 2)))
data$f = rgp(10) + 1:10
names(data$f) <- letters[1:10]
data$fi = jiggle(data$f)
data$fb = tfb(data$f)

ggplot(data, aes(tf = f, color = depth(f))) + geom_spaghetti() +
  annotate("text", x = 1.05, y = drop(data$f[, 1]), label = names(data$f))
ggplot(data, aes(tf = fi, shape = col, color = col)) + geom_meatballs()
ggplot(data, aes(tf = fi)) + geom_meatballs(spaghetti = FALSE) +
  facet_wrap(~col)

# geom_lasagna is a hack of geom_line because geom_tile won't accept the
# id-factor as a vertical axis -- adjust line width ("size") to get a proper
# lasagna without gaps just like nonna used to make back in the old country:
ggplot(data, aes(tf = f, order = col)) + geom_lasagna(size = 8)
ggplot(data, aes(tf = fb, order = col)) + geom_lasagna(size = 12)

# todo: ggplot2 drops all names :( - get decent lasagna labels how?


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
ggplot(data, aes(tf = f, order = col)) + geom_lasagna(order_by = function(f) f[1])
# .. but facetting is broken for lasagna:
ggplot(data, aes(tf = f, order = col)) +
  geom_lasagna()  + facet_wrap(~ col)

