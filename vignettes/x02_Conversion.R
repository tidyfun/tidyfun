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
library(SemiPar)
library(gridExtra)

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

## -----------------------------------------------------------------------------
dti_df <- tibble(
  id = refund::DTI$ID,
  visit = refund::DTI$visit,
  sex = refund::DTI$sex,
  case = factor(ifelse(refund::DTI$case, "MS", "control"))
)

dti_df$cca <- tfd(refund::DTI$cca, arg = seq(0, 1, length.out = 93))
dti_df$rcst <- tfd(refund::DTI$rcst, arg = seq(0, 1, length.out = 55))

## -----------------------------------------------------------------------------
dti_df

## -----------------------------------------------------------------------------
dti_df |>
  ggplot() +
  geom_spaghetti(aes(tf = cca, col = case, alpha = 0 + (case == "control"))) +
  facet_wrap(~sex) +
  scale_alpha(guide = "none", range = c(0.2, 0.4))

## -----------------------------------------------------------------------------
canada <- tibble(
  place = fda::CanadianWeather$place,
  region = fda::CanadianWeather$region,
  lat = fda::CanadianWeather$coordinates[, 1],
  lon = -fda::CanadianWeather$coordinates[, 2]
) |>
  mutate(
    temp = t(fda::CanadianWeather$dailyAv[,, 1]) |>
      tfd(arg = 1:365),
    precipl10 = t(fda::CanadianWeather$dailyAv[,, 3]) |>
      tfd(arg = 1:365) |>
      tf_smooth()
  )

## -----------------------------------------------------------------------------
canada

## -----------------------------------------------------------------------------
temp_panel <- canada |>
  ggplot(aes(tf = temp, color = region)) +
  geom_spaghetti()

precip_panel <- canada |>
  ggplot(aes(tf = precipl10, color = region)) +
  geom_spaghetti()

gridExtra::grid.arrange(temp_panel, precip_panel, nrow = 1)

## -----------------------------------------------------------------------------
data("pig.weights", package = "SemiPar")

pig.weights <- as_tibble(pig.weights)

pig.weights

## -----------------------------------------------------------------------------
pig_df <- pig.weights |>
  tf_nest(weight, .id = id.num, .arg = num.weeks)

pig_df

## -----------------------------------------------------------------------------
pig_df |>
  ggplot(aes(tf = weight)) +
  geom_spaghetti()

## ----eval = FALSE-------------------------------------------------------------
# install.packages("ALA", repos = "http://R-Forge.R-project.org")

## ----eval = FALSE-------------------------------------------------------------
# ALA::fev1 |> glimpse()
# ## Rows: 1,994
# ## Columns: 6
# ## $ id      <fct> 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, …
# ## $ age     <dbl> 9.3415, 10.3929, 11.4524, 12.4600, 13.4182, 15.4743, 16.3723, 6.5873, 7.6496, 12.7392, 13.7741, 14.6940, 15.…
# ## $ height  <dbl> 1.20, 1.28, 1.33, 1.42, 1.48, 1.50, 1.52, 1.13, 1.19, 1.49, 1.53, 1.55, 1.56, 1.57, 1.57, 1.18, 1.23, 1.30, …
# ## $ age0    <dbl> 9.3415, 9.3415, 9.3415, 9.3415, 9.3415, 9.3415, 9.3415, 6.5873, 6.5873, 6.5873, 6.5873, 6.5873, 6.5873, 6.58…
# ## $ height0 <dbl> 1.20, 1.20, 1.20, 1.20, 1.20, 1.20, 1.20, 1.13, 1.13, 1.13, 1.13, 1.13, 1.13, 1.13, 1.13, 1.18, 1.18, 1.18, …
# ## $ logFEV1 <dbl> 0.21511, 0.37156, 0.48858, 0.75142, 0.83291, 0.89200, 0.87129, 0.30748, 0.35066, 0.75612, 0.86710, 1.04732, …
# ALA::fev1 |>
#   group_by(id) |>
#   mutate(n_obs = n()) |>
#   filter(n_obs > 1) |>
#   tf_nest(logFEV1, height, .arg = age) |>
#   glimpse()
# ## Rows: 252
# ## Columns: 6
# ## $ id      <fct> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 16, 18, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 3…
# ## $ age0    <dbl> 9.3415, 6.5873, 6.9131, 6.7598, 6.5024, 6.8994, 6.4339, 7.1869, 6.8966, 7.7892, 7.6140, 7.5483, 7.8412, 6.50…
# ## $ height0 <dbl> 1.20, 1.13, 1.18, 1.15, 1.11, 1.24, 1.18, 1.27, 1.17, 1.13, 1.32, 1.25, 1.25, 1.20, 1.19, 1.24, 1.21, 1.23, …
# ## $ n_obs   <int> 7, 8, 9, 10, 7, 11, 7, 9, 9, 10, 6, 3, 5, 11, 12, 10, 9, 8, 12, 2, 2, 11, 11, 7, 9, 11, 11, 4, 2, 12, 3, 9, …
# ## $ logFEV1 <tfd_irrg> [<9.3415, 10.3929, 11.4524, 12.4600, 13.4182, 15.4743, 16.3723>, <0.21511, 0.37156, 0.48858, 0.75142, 0…
# ## $ height  <tfd_irrg> [<9.3415, 10.3929, 11.4524, 12.4600, 13.4182, 15.4743, 16.3723>, <1.20, 1.28, 1.33, 1.42, 1.48, 1.50, 1…

## -----------------------------------------------------------------------------
dti_df <- refund::DTI |>
  janitor::clean_names() |>
  select(-starts_with("rcst")) |>
  glimpse()

dti_df |>
  tf_gather(starts_with("cca")) |>
  glimpse()

## -----------------------------------------------------------------------------
pig_df |>
  tf_unnest(cols = weight) |>
  glimpse()

## -----------------------------------------------------------------------------
pig_df |>
  tf_spread() |>
  glimpse()

## -----------------------------------------------------------------------------
weight_vec <- pig_df$weight

weight_matrix <- weight_vec |> as.matrix()

head(weight_matrix)

# argument values of input data saved in `arg`-attribute:
attr(weight_matrix, "arg")

## -----------------------------------------------------------------------------
weight_vec

weight_vec |>
  as.data.frame(unnest = TRUE) |>
  head()
