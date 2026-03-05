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
# try(devtools::load_all("~/fda/tidyfun"))
# try(devtools::load_all("~/Work/fda/tidyfun"))

pal_5 <- viridis(7)[-(1:2)]
set.seed(1221)

## ----view_chf-----------------------------------------------------------------
data(chf_df)

chf_df

## ----plot_chf-----------------------------------------------------------------
chf_df |>
  slice(1:5) |>
  ggplot(aes(tf = activity)) +
  geom_spaghetti(alpha = 0.1)

## ----view_dti-----------------------------------------------------------------
data(dti_df)

dti_df

## ----plot_dti-----------------------------------------------------------------
dti_df |>
  ggplot(aes(tf = cca)) +
  geom_spaghetti(alpha = 0.05)

## -----------------------------------------------------------------------------
chf_df |>
  select(id, day, activity) |>
  filter(day == "Mon") |>
  ggplot(aes(tf = activity)) +
  geom_spaghetti(alpha = 0.05)

## -----------------------------------------------------------------------------
chf_df |>
  group_by(day) |>
  summarize(mean_act = mean(activity)) |>
  ggplot(aes(tf = mean_act, color = day)) +
  geom_spaghetti()

## -----------------------------------------------------------------------------
chf_df |>
  slice(1:5) |>
  mutate(exp_act = exp(activity)) |>
  ggplot(aes(tf = exp_act)) +
  geom_spaghetti(alpha = 0.2)

## -----------------------------------------------------------------------------
chf_df |>
  select(id, day, activity) |>
  pivot_wider(
    names_from = day,
    values_from = activity
  )

## -----------------------------------------------------------------------------
monday_df <- chf_df |>
  filter(day == "Mon") |>
  select(id, monday_act = activity)
friday_df <- chf_df |>
  filter(day == "Fri") |>
  select(id, friday_act = activity)

## -----------------------------------------------------------------------------
monday_df |>
  left_join(friday_df, by = "id") |>
  pivot_longer(monday_act:friday_act, names_to = "day", values_to = "activity")

## -----------------------------------------------------------------------------
groups_dti <- dti_df |>
  group_by(case, sex) |>
  summarize(mean_rcst = mean(rcst, na.rm = TRUE))
groups_dti

ggplot(groups_dti, aes(tf = mean_rcst, color = case)) +
  geom_spaghetti(linewidth = 2) +
  facet_grid(~sex)

## -----------------------------------------------------------------------------
like_to_move_it_move_it <- chf_df |> filter(tf_anywhere(activity, value > 9))
glimpse(like_to_move_it_move_it)

like_to_move_it_move_it |>
  ggplot(aes(tf = activity)) +
  geom_spaghetti(aes(colour = id)) +
  theme(legend.position = "none")

## -----------------------------------------------------------------------------
dti_df |>
  filter(tf_anywhere(cca, value < 0.26)) |>
  ggplot(aes(tf = cca)) +
  geom_spaghetti() +
  # add entire data in light grey
  geom_spaghetti(data = dti_df, alpha = .01)

## -----------------------------------------------------------------------------
chf_df |>
  filter(id == 1) |>
  mutate(smooth_act = tf_smooth(activity)) |>
  ggplot(aes(tf = smooth_act)) +
  geom_spaghetti()

## -----------------------------------------------------------------------------
chf_df |>
  group_by(day) |>
  summarize(mean_act = mean(activity)) |>
  mutate(smooth_mean = tf_smooth(mean_act)) |>
  ggplot(aes(tf = mean_act, color = day)) +
  geom_spaghetti(alpha = 0.2) +
  geom_spaghetti(aes(tf = smooth_mean), linewidth = 2)

## -----------------------------------------------------------------------------
chf_df |>
  filter(id == 1) |>
  mutate(daytime_act = tf_zoom(activity, 360, 1200)) |>
  ggplot(aes(tf = daytime_act)) +
  geom_spaghetti(alpha = 0.2)

## -----------------------------------------------------------------------------
dti_df <- dti_df |> mutate(cca_tfb = tfb(cca, k = 25))

## -----------------------------------------------------------------------------
dti_df |>
  slice(1:10) |>
  mutate(
    cca_raw_deriv = tf_derive(cca),
    cca_tfb_deriv = tf_derive(cca_tfb)
  ) |>
  ggplot() +
  geom_spaghetti(
    aes(tf = cca_raw_deriv),
    alpha = 0.3,
    linewidth = 0.3,
    col = "blue"
  ) +
  geom_spaghetti(
    aes(tf = cca_tfb_deriv),
    alpha = 0.3,
    linewidth = 0.3,
    col = "red"
  ) +
  ylab("d/dt f(t)")
