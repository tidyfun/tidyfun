# Changelog

## tidyfun 0.2.0

CRAN release: 2026-07-16

- Trajectory plots of `tf_mv` objects (and all other
  [`tf_ggplot()`](https://tidyfun.github.io/tidyfun/reference/tf_ggplot.md)
  layers) now provide a column `.arg` with the argument value of each
  evaluation point, so `aes(colour = .arg)` colours curve segments by
  their position along the domain.
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)/[`autolayer()`](https://ggplot2.tidyverse.org/reference/autolayer.html)
  for `tf_mv` objects gain a `colour_by_arg` argument for the same
  purpose.
- New
  [`tf_phaseplane()`](https://tidyfun.github.io/tidyfun/reference/tf_phaseplane.md)
  for phase-plane plots (as in
  [`fda::phaseplanePlot()`](https://rdrr.io/pkg/fda/man/phaseplanePlot.html)):
  turns a univariate `tf` into a two-component `tf_mv` of derivatives
  (velocity vs. acceleration by default) that can be plotted as a
  trajectory with
  [`tf_ggplot()`](https://tidyfun.github.io/tidyfun/reference/tf_ggplot.md)
  or
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).
- `ggplot2` support for multivariate functional data (`tf_mv` columns,
  see
  [`tf::tfd_mv()`](https://tidyfun.github.io/tf/reference/tfd_mv.html)):
  map them with `aes(tf = ...)` in
  [`tf_ggplot()`](https://tidyfun.github.io/tidyfun/reference/tf_ggplot.md),
  with `type = "facet"` (value-vs-arg curves, one panel per output
  dimension) or `type = "trajectory"` (planar curves x(t) vs y(t) for
  2-component objects).
- New
  [`autoplot.tf_mv()`](https://tidyfun.github.io/tidyfun/reference/autoplot.tf_mv.md)
  and
  [`autolayer.tf_mv()`](https://tidyfun.github.io/tidyfun/reference/autoplot.tf_mv.md)
  methods for quick plots and layers of `tf_mv` objects.
- New
  [`tf_unnest.tf_mv()`](https://tidyfun.github.io/tidyfun/reference/tf_unnest.md)
  method returning a “wide” long table with one value column per output
  dimension: `(id, arg, <component 1>, ..., <component d>)`.
- Behavior change: an explicitly mapped `x` aesthetic is no longer
  silently overwritten by `tf_y`’s `arg` grid, so
  `aes(tf_x = fx, tf_y = fy)` now correctly draws planar curves x(t) vs
  y(t).
- Univariate-only displays
  ([`geom_spaghetti()`](https://tidyfun.github.io/tidyfun/reference/ggspaghetti.md)/[`geom_meatballs()`](https://tidyfun.github.io/tidyfun/reference/ggspaghetti.md),
  [`gglasagna()`](https://tidyfun.github.io/tidyfun/reference/gglasagna.md),
  [`geom_fboxplot()`](https://tidyfun.github.io/tidyfun/reference/ggfboxplot.md))
  now fail early with an informative error for `tf_mv` inputs instead of
  dying with obscure internal errors.

## tidyfun 0.1.2

CRAN release: 2026-04-24

- Rebuilt `chf_df` with current `tf` constructors before saving so the
  packaged dataset no longer carries stale namespace references from
  pre-`tf` releases.

## tidyfun 0.1.1

CRAN release: 2026-04-13

- Initial CRAN submission.
- Functional data types (`tfd`, `tfb`) as data frame columns via the
  `tf` package.
- `tidyverse`-compatible data wrangling: `tf_gather`, `tf_spread`,
  `tf_nest`, `tf_unnest`.
- `ggplot2` geoms for functional data: `geom_spaghetti`,
  `geom_meatballs`, `geom_capellini`, `geom_errorband`, `gglasagna`.
- [`tf_ggplot()`](https://tidyfun.github.io/tidyfun/reference/tf_ggplot.md)
  for tf-aware ggplot construction with standard geoms.
- Functional boxplots via `geom_fboxplot`.
