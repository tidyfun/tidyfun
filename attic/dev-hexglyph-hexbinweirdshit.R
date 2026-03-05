library(ggplot2)

set.seed(131112)
data <- data.frame(x = rnorm(1e2), y = rnorm(1e2))

p1 <- ggplot(data, aes(x = x, y = y)) +
  geom_hex(bins = 10) +
  geom_point(col = "yellow")
# add red cross for hex-center:
p2 <- p1 + geom_point(data = layer_data(p1), col = "red", shape = 3)

# adding the point-layer changes the location/number of the hexes !?!
all.equal(layer_data(p1, 1), layer_data(p2, 1))
gridExtra::grid.arrange(p1, p2, nrow = 2)

# layer_data *should* contain the centers of the hexagons, IIUC,
# but that's not the case --
# 1. they are not in the center of the hex, mostly
# 2. not every hex is represented in layer_data and not every point in layer_data is in a hex....

# for this toy example, I can recover what I want by extracting the updated hex info from p2 and
# then creating a third plot based on p1 with that, but that's ugly, I'm uncertain how reliable this will be,
# and it won't work for my actual use case in which I need to do more complicated things
# based on which point is in which hex.....
p3 <- p1 + geom_point(data = layer_data(p2, 1), col = "orange", shape = 3)
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)


# if I manually set axis limits, it seems to work, so it seems to have something to do with axes being recalculated..?
p_limits <- ggplot(data, aes(x = x, y = y)) +
  geom_hex(bins = 10) +
  xlim(c(-3, 4)) +
  ylim(c(-4, 3))
p_limits
p_limits + geom_point(data = layer_data(p_limits), col = "red")

# ... not for coord_fixed, though
p_coord_fixed <- ggplot(data, aes(x = x, y = y)) +
  geom_hex(bins = 10) +
  coord_fixed(ratio = 1)
p_coord_fixed
p_coord_fixed + geom_point(data = layer_data(p_coord_fixed), col = "red")
