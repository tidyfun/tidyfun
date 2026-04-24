###########################################################
# File imports and organizes activity data from a study of
# congestive heart failure. Data were originally presented
# in "Multilevel Matrix-Variate Analysis and its Application
# to Accelerometry-Measured Physical Activity in Clinical
# Populations" by Huang et al.; these data are public,
# with download information in the paper.
#
# We focus on a subset to avoid issues of memory.
###########################################################

library(tidyfun)
library(readr)
library(dplyr)

covar <- read_csv(here::here("data-raw", "covariate.csv"))
activity <- read_csv(here::here("data-raw", "activity.csv"))

chf_df <- inner_join(covar, filter(activity, week == 1), by = "id") |>
  tf_gather(activity.1:activity.1440, key = activity) |>
  select(-week) |>
  mutate(
    day = ordered(
      day,
      levels = c(
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
        "Sunday"
      ),
      labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    )
  )

# Rebuild the tf column with the current tf constructors so the serialized
# dataset only references current strong dependencies.
activity_old <- chf_df$activity
evaluator_name <- attr(activity_old, "evaluator_name")
chf_df$activity <- eval(
  bquote(
    tf::tfd(
      tf::tf_evaluations(activity_old),
      arg = tf::tf_arg(activity_old),
      domain = tf::tf_domain(activity_old),
      evaluator = .(as.name(evaluator_name))
    )
  )
)
rm(activity_old, evaluator_name)

usethis::use_data(chf_df, overwrite = TRUE)
