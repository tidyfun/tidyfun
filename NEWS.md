# tidyfun (development version)

* `ggplot2` support for multivariate functional data (`tf_mv` columns, see
  `tf::tfd_mv()`): map them with `aes(tf = ...)` in `tf_ggplot()`, with
  `type = "facet"` (value-vs-arg curves, one panel per output dimension) or
  `type = "trajectory"` (planar curves x(t) vs y(t) for 2-component objects).
* New `autoplot.tf_mv()` and `autolayer.tf_mv()` methods for quick plots and
  layers of `tf_mv` objects.
* New `tf_unnest.tf_mv()` method returning a "wide" long table with one value
  column per output dimension: `(id, arg, <component 1>, ..., <component d>)`.
* Behavior change: an explicitly mapped `x` aesthetic is no longer silently
  overwritten by `tf_y`'s `arg` grid, so `aes(tf_x = fx, tf_y = fy)` now
  correctly draws planar curves x(t) vs y(t).
* Univariate-only displays (`geom_spaghetti()`/`geom_meatballs()`,
  `gglasagna()`, `geom_fboxplot()`) now fail early with an informative error
  for `tf_mv` inputs instead of dying with obscure internal errors.

# tidyfun 0.1.2

* Rebuilt `chf_df` with current `tf` constructors before saving so the packaged
  dataset no longer carries stale namespace references from pre-`tf` releases.

# tidyfun 0.1.1

* Initial CRAN submission.
* Functional data types (`tfd`, `tfb`) as data frame columns via the `tf` package.
* `tidyverse`-compatible data wrangling: `tf_gather`, `tf_spread`, `tf_nest`,
  `tf_unnest`.
* `ggplot2` geoms for functional data: `geom_spaghetti`, `geom_meatballs`,
  `geom_capellini`, `geom_errorband`, `gglasagna`.
* `tf_ggplot()` for tf-aware ggplot construction with standard geoms.
* Functional boxplots via `geom_fboxplot`.
