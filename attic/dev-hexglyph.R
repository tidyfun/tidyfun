#+  warning = FALSE

library(tidyverse)
devtools::load_all("~/fda/tidyfun") #tidyfun

grid <- seq(-pi, pi, l = 51)
n <- 350
f <- replicate(n, rnorm(1) * sin(grid) + rnorm(1) * cos(grid)) |>
  t() |>
  tfd(grid)
scores <- f |> as.matrix() |> dist() |> cmdscale(k = 3)

testdata <- tibble(f = f, x1 = scores[, 1], x2 = scores[, 2], x3 = scores[, 3])

ggplot(testdata, aes(x = x1, y = x2)) + geom_point()
ggplot(testdata, aes(x = x1, y = x2, tf = f)) +
  geom_capelini(
    width = diff(range(testdata$x1)) / sqrt(n),
    height = diff(range(testdata$x2)) / sqrt(n),
    box.fill = "white",
    box.alpha = .6
  )


data <- testdata |> rename(x = "x1", y = "x2")

hexglyph_2d <- function(
  data, #a dataframe with columns "x, y" for dimred-scores and corresponding tfd-vector column "f"
  bins = 10, #bin-count for geom_hex
  color = "white",
  width,
  height,
  size = 1,
  line.colour = "grey",
  line.size = 1,
  line.linetype = 1,
  ...
) {
  # 1. compute & plot hexbin-plot
  p1 <- ggplot(data, aes(x = x, y = y)) +
    geom_hex(bins = bins) +
    scale_fill_viridis_c()
  #would need to set fixed limits to avoid new limits being computed when glyphs are
  #added, which invalidates the layer_data info about where the hexes are
  #located...
  p1_info <- ggplot_build(p1)
  #p1 <- p1 +  xlim(extendrange(p1_info$layout$panel_params[[1]]$x.range)) +
  #  ylim(extendrange(p1_info$layout$panel_params[[1]]$y.range))

  #coord_cartesian(
  #xlim = p1_info$layout$panel_scales_x[[1]]$range$range,
  #ylim = p1_info$layout$panel_scales_y[[1]]$range$range)

  # 2. extract  hex-centers
  hexdata <- layer_data(p1)
  # 3. match funs to hex-centers

  ##  line below would be easy, but it's not exact enough...?!?
  #! hexgroups <- class::knn(train = hexdata[, c("x", "y")], test = data[, c("x", "y")], cl = 1:nrow(hexdata), k =1)
  #! all(table(hexgroups) == hexdata$count) #sanity check fails

  ## instead re-compute internal hexbin settings, see ggplot2/R/hexbin.R
  # binwidth will be wrong if ylim, xlim are set manually
  binwidth <- c(
    diff(range(data$x)) / bins,
    diff(range(data$y)) / bins
  )
  xbnds <- ggplot2:::hex_bounds(data$x, binwidth[1])
  xbins <- diff(xbnds) / binwidth[1]
  ybnds <- ggplot2:::hex_bounds(data$y, binwidth[2])
  ybins <- diff(ybnds) / binwidth[2]
  hb <- hexbin::hexbin(
    data$x,
    xbnds = xbnds,
    xbins = xbins,
    data$y,
    ybnds = ybnds,
    shape = ybins / xbins,
    IDs = TRUE
  )
  # turn non-consec. cIDs into consecutive integers
  hexgroups <- match(hb@cID, sort(unique(hb@cID)))
  stopifnot(all(table(hexgroups) == hexdata$count))

  # 4. compute hex-wise medians
  hexmedians <- data |>
    mutate(hex = hexgroups) |>
    group_by(hex) |>
    summarise(f = median(f)) |>
    bind_cols(hexdata[, c("x", "y")])

  # 5. plot cappelini of hex-medians
  if (missing(width)) width <- .9 * min(diff(sort(unique(hexdata$x))))
  if (missing(height)) height <- .9 * min(diff(sort(unique(hexdata$y))))
  p2 <- p1 +
    geom_capelini(
      data = hexmedians,
      aes(x = x, y = y, tf = f),
      size = size,
      color = color,
      width = width,
      height = height,
      line.colour = line.colour,
      line.size = line.size,
      line.linetype = line.linetype,
      ...
    )
  p2
}

hexglyph_2d(data, bins = 4, alpha = 1)
hexglyph_2d(data, bins = 8, alpha = 1)
hexglyph_2d(data, bins = 12, alpha = 1)
hexglyph_2d(data, bins = 15, alpha = 1)

####################

library(patchwork)

f <- read.table("~/data/ECG200/ECG200_TRAIN.txt") |> as.matrix() |> tfd() # |> tf_smooth()
scores <- f |> as.matrix() |> dist() |> cmdscale(k = 3)

data <- tibble(
  f = unname(f),
  x1 = scores[, 1],
  x2 = scores[, 2],
  x3 = scores[, 3]
)
p1 <- hexglyph_2d(data |> rename(x = "x1", y = "x2"), bins = 8, alpha = 1) +
  theme(legend.position = "none")
p2 <- hexglyph_2d(data |> rename(x = "x1", y = "x3"), bins = 8, alpha = 1) +
  theme(legend.position = "none")
p3 <- hexglyph_2d(data |> rename(x = "x2", y = "x3"), bins = 8, alpha = 1) +
  theme(legend.position = "none")
p4 <- ggplot(data) + geom_spaghetti(aes(y = f))

(p1 + p2) / (p4 + p3)
