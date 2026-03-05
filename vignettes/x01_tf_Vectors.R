## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "##",
  fig.width = 8,
  fig.height = 5.5,
  out.width = "90%"
)

library("tidyverse")
library("viridisLite")

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)
scale_colour_discrete <- scale_colour_viridis_d
scale_fill_discrete <- scale_fill_viridis_d

library("tidyfun")

pal_5 <- viridis(7)[-(1:2)]
set.seed(1221)

## ----load-dti-----------------------------------------------------------------
data("dti_df")

cca <- dti_df$cca
cca

## ----ex-def-------------------------------------------------------------------
cca_five <- cca[1:5, seq(0, 1, length.out = 93), interpolate = TRUE]
rownames(cca_five) <- LETTERS[1:5]
cca_five <- tfd(cca_five, signif = 2)
cca_five

## ----ex-fig-------------------------------------------------------------------
plot(cca_five, xlim = c(-0.15, 1), col = pal_5)
text(x = -0.1, y = cca_five[, 0.07], labels = names(cca_five), col = pal_5)

## -----------------------------------------------------------------------------
cca_five |>
  tf_evaluations() |>
  str()
cca_five |>
  tf_arg() |>
  str()
cca_five |> tf_domain()

## -----------------------------------------------------------------------------
tf_evaluator(cca_five) |> str()
tf_evaluator(cca_five) <- tf_approx_spline

## -----------------------------------------------------------------------------
cd4_vec <- tfd(refund::cd4)

cd4_vec[1:2]
cd4_vec[1:2] |>
  tf_arg() |>
  str()
cd4_vec[1:20] |> plot(pch = "x", col = viridis(20))

## -----------------------------------------------------------------------------
refund::DTI$cca |>
  object.size() |>
  print(units = "Kb")
cca |>
  object.size() |>
  print(units = "Kb")
cca |>
  tfb(verbose = FALSE) |>
  object.size() |>
  print(units = "Kb")

## ----message = TRUE-----------------------------------------------------------
cca_five_b <- cca_five |> tfb()
cca_five_b[1:2]
cca_five[1:2] |> tfb(bs = "tp", k = 55)

# functions represent ratios in (0,1), so a Beta-distribution is more appropriate:
cca_five[1:2] |>
  tfb(bs = "ps", m = c(2, 1), family = mgcv::betar(link = "cloglog"))

## -----------------------------------------------------------------------------
layout(t(1:2))
cca_five |> plot()
cca_five_b |> plot(col = "red")
cca_five |>
  tfb(k = 35, penalized = FALSE) |>
  lines(col = "blue")
cca_five |>
  tfb(sp = 0.001) |>
  lines(col = "orange")

## ----echo = FALSE-------------------------------------------------------------
set.seed(1212)
raw <- c(
  tf_rgp(5, scale = 0.2, nugget = 0.05, arg = 101L) - 5,
  tf_rgp(5, scale = 0.02, nugget = 0.05, arg = 101L),
  tf_rgp(5, scale = 0.002, nugget = 0.05, arg = 101L) + 5
)

## -----------------------------------------------------------------------------
layout(t(1:3))
clrs <- scales::alpha(sample(viridis(15)), 0.5)
plot(raw, main = "raw", col = clrs)
plot(tfb(raw, k = 55), main = "separate", col = clrs)
plot(tfb(raw, k = 55, global = TRUE), main = "global", col = clrs)

## -----------------------------------------------------------------------------
cca_five_fpc <- cca_five |> tfb_fpc(pve = 0.999)
cca_five_fpc

cca_five_fpc_lowrank <- cca_five |> tfb_fpc(pve = 0.6)
cca_five_fpc_lowrank

## -----------------------------------------------------------------------------
layout(t(1:2))
cca_five |> plot()
cca_five_fpc |> plot(col = "red", ylab = "tfb_fpc(cca_five)")
cca_five_fpc_lowrank |> lines(col = "blue", lty = 2)

## -----------------------------------------------------------------------------
cca_five[1:2]
cca_five[1:2] <- cca_five[2:1]
cca_five

## ----echo = FALSE-------------------------------------------------------------
n_cca_five <- names(cca_five)
cca_five <- unname(cca_five)

## -----------------------------------------------------------------------------
cca_five[1] + cca_five[1] == 2 * cca_five[1]
log(exp(cca_five[2])) == cca_five[2]
(cca_five - (2:-2)) != cca_five

## ----echo = FALSE-------------------------------------------------------------
names(cca_five) <- n_cca_five

## -----------------------------------------------------------------------------
c(mean = mean(cca_five), sd = sd(cca_five))

tf_depth(cca_five) ## Modified Band-2 Depth (à la Sun/Genton/Nychka, 2012), others to come.
median(cca_five) == cca_five[which.max(tf_depth(cca_five))]
summary(cca_five)

## -----------------------------------------------------------------------------
tf_fmean(cca_five) # mean of each function's evaluations
tf_fmax(cca_five) # max of each function's evaluations
# 25%-tile of each f(t) for t > .5:
tf_fwise(cca_five, \(x) quantile(x$value[x$arg > 0.5], prob = 0.25)) |> unlist()

## ----warning  = FALSE---------------------------------------------------------
cca_five[1:2, seq(0, 1, length.out = 3)]
cca_five["B", seq(0, 0.15, length.out = 3), interpolate = FALSE]
cca_five[1:2, seq(0, 1, length.out = 7), matrix = FALSE] |> str()

## -----------------------------------------------------------------------------
layout(t(1:3))
cca_five |> plot(alpha = 0.2, ylab = "lowess")
cca_five |>
  tf_smooth("lowess") |>
  lines(col = pal_5)

cca_five |> plot(alpha = 0.2, ylab = "rolling median (k=5)")
cca_five |>
  tf_smooth("rollmedian", k = 5) |>
  lines(col = pal_5)

cca_five |> plot(alpha = 0.2, ylab = "Savitzky-Golay (quartic, 11 steps)")
cca_five |>
  tf_smooth("savgol", fl = 11) |>
  lines(col = pal_5)

## -----------------------------------------------------------------------------
layout(t(1:3))
cca_five |> plot(col = pal_5)
cca_five |>
  tf_smooth() |>
  tf_derive() |>
  plot(col = pal_5, ylab = "tf_derive(tf_smooth(cca_five))")
cca_five |>
  tf_integrate(definite = FALSE) |>
  plot(col = pal_5)

## -----------------------------------------------------------------------------
cca_five |> tf_integrate()

## -----------------------------------------------------------------------------
cca_five |> tf_anywhere(value > 0.65)
cca_five[1:2] |> tf_where(value > 0.6, "all")
cca_five[2] |> tf_where(value > 0.6, "range")
cca_five |> tf_where(value > 0.6 & arg > 0.5, "first")

## ----ex-fig2------------------------------------------------------------------
cca_five |> plot(xlim = c(-0.15, 1), col = pal_5, lwd = 2)
text(
  x = -0.1,
  y = cca_five[, 0.07],
  labels = names(cca_five),
  col = pal_5,
  cex = 1.5
)
median(cca_five) |> lines(col = pal_5[3], lwd = 4)

## -----------------------------------------------------------------------------
# where are the first maxima of these functions?
cca_five |> tf_where(value == max(value), "first")

# where are the first maxima of the later part (t > .5) of these functions?
cca_five[c("A", "D")] |>
  tf_zoom(0.5, 1) |>
  tf_where(value == max(value), "first")

# which f_i(t) are below the functional median anywhere for 0.2 < t < 0.6?
# (t() needed here so we're comparing column vectors to column vectors...)
cca_five |>
  tf_zoom(0.2, 0.6) |>
  tf_anywhere(value <= t(median(cca_five)[, arg]))
