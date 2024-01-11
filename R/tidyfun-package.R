#' @description
#' `tidyfun` makes data wrangling and exploratory analysis for functional data
#' in `R` easier.\cr\cr
#' `tidyfun` is based on the classes and methods defined in package `tf` and provides:
#'
#'   - new data types for representing functional data: [tf::tfd()] & [tf::tfb()]
#'   - arithmetic operators ([tf::Ops.tf()]),
#'   - descriptive statistics: e.g. [tf::mean.tf()], [tf::median.tf()]
#'   - and graphics functions for such data: [tf::plot.tf()], [geom_spaghetti()], [gglasagna()]
#'   - functions to do smoothing ([tf::tf_smooth.tfd()]), differentiation and integration ([tf::tf_derive.tfd()])
#' - `tidyverse`-verbs for handling functional data inside data frames: [tf_gather()] etc.
#' \cr\cr
#' \cr
#' Also see `vignette("Introducing tidyfun", "tidyfun")` for a brief introduction.
#'
#' @keywords internal
"_PACKAGE"
