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
