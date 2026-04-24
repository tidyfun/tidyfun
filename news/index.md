# Changelog

## tidyfun 0.1.2

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
