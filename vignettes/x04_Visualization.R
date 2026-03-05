## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "##",
  fig.width = 8,
  fig.height = 5.5,
  out.width = "90%"
)

library(tidyverse)
library(viridisLite)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)
scale_colour_discrete <- scale_colour_viridis_d
scale_fill_discrete <- scale_fill_viridis_d

library("tidyfun")
data(chf_df, package = "tidyfun")
data(dti_df, package = "tidyfun")

pal_5 <- viridis(7)[-(1:2)]
set.seed(1221)

## -----------------------------------------------------------------------------
cca <- dti_df$cca |>
  tfd(arg = seq(0, 1, length.out = 93), interpolate = TRUE)

layout(t(1:2))

plot(cca, type = "spaghetti")
lines(c(median(cca), mean = mean(cca)), col = c(2, 4))

plot(cca, type = "lasagna", col = viridis(50))

## ----ex-fig2------------------------------------------------------------------
cca_five <- cca[1:5]

cca_five |> plot(xlim = c(-0.15, 1), col = pal_5, lwd = 2)

text(
  x = -0.1,
  y = cca_five[, 0.07],
  labels = names(cca_five),
  col = pal_5,
  cex = 1.5
)

median(cca_five) |> lines(col = pal_5[3], lwd = 4)

## ----plot_chf-----------------------------------------------------------------
chf_df |>
  filter(id == 1) |>
  ggplot(aes(tf = tf_smooth(activity))) +
  geom_spaghetti()

## -----------------------------------------------------------------------------
chf_df |>
  filter(id == 1, day == "Mon") |>
  ggplot(aes(tf = activity)) +
  geom_polpette()

## -----------------------------------------------------------------------------
chf_df |>
  filter(day == "Mon") |>
  ggplot(aes(tf = tf_smooth(activity), color = gender)) +
  geom_spaghetti(alpha = 0.3)

## -----------------------------------------------------------------------------
chf_df |>
  filter(id < 20, day %in% c("Mon", "Sun")) |>
  ggplot(aes(tf = tf_smooth(activity), color = gender)) +
  geom_spaghetti(alpha = 0.5) +
  facet_grid(~day)

## ----dti-fig1-----------------------------------------------------------------
dti_df |>
  ggplot() +
  geom_spaghetti(aes(
    tf = cca,
    col = case,
    alpha = 0.2 + 0.4 * (case == "control")
  )) +
  facet_wrap(~sex) +
  scale_alpha(guide = "none", range = c(0.2, 0.4))

## -----------------------------------------------------------------------------
chf_df |>
  group_by(gender, day) |>
  summarize(mean_act = mean(activity)) |>
  mutate(smooth_mean = tfb(mean_act, verbose = FALSE)) |>
  filter(day %in% c("Mon", "Sun")) |>
  ggplot(aes(tf = smooth_mean, color = gender)) +
  geom_spaghetti(linewidth = 1.25, alpha = 1) +
  geom_polpette(aes(tf = mean_act), alpha = 0.1) +
  facet_grid(~day)

## -----------------------------------------------------------------------------
chf_df |>
  group_by(gender, day) |>
  summarize(
    mean_act = mean(activity),
    sd_act = sd(activity)
  ) |>
  group_by(gender, day) |>
  mutate(
    upper_act = mean_act + 2 * sd_act,
    lower_act = mean_act - 2 * sd_act
  ) |>
  filter(day %in% c("Mon", "Sun")) |>
  ggplot(aes(tf = mean_act, color = gender, fill = gender)) +
  geom_spaghetti(alpha = 1) +
  geom_papardelle(aes(fmax = upper_act, fmin = lower_act), alpha = 0.3) +
  facet_grid(gender ~ day)

## -----------------------------------------------------------------------------
chf_df |>
  filter(day %in% c("Mon", "Sun")) |>
  gglasagna(activity)

## ----dti-fig2-----------------------------------------------------------------
dti_df |>
  gglasagna(
    tf = cca,
    order = tf_integrate(cca, definite = TRUE),
    arg = seq(0, 1, length.out = 101)
  ) +
  theme(axis.text.y = element_text(size = 6)) +
  facet_wrap(~case, ncol = 2, scales = "free")

## -----------------------------------------------------------------------------
canada <- data.frame(
  place = fda::CanadianWeather$place,
  region = fda::CanadianWeather$region,
  lat = fda::CanadianWeather$coordinates[, 1],
  lon = -fda::CanadianWeather$coordinates[, 2]
)

canada$temp <- tfd(t(fda::CanadianWeather$dailyAv[,, 1]), arg = 1:365)
canada$precipl10 <- tfd(t(fda::CanadianWeather$dailyAv[,, 3]), arg = 1:365) |>
  tf_smooth()

canada_map <-
  data.frame(maps::map("world", "Canada", plot = FALSE)[c("x", "y")])

## -----------------------------------------------------------------------------
ggplot(canada, aes(x = lon, y = lat)) +
  geom_capellini(
    aes(tf = precipl10),
    width = 4,
    height = 3,
    colour = "blue",
    line.linetype = 1
  ) +
  geom_capellini(
    aes(tf = temp),
    width = 4,
    height = 3,
    colour = "red",
    line.linetype = 1
  ) +
  geom_path(data = canada_map, aes(x = x, y = y), alpha = 0.1) +
  coord_quickmap()
