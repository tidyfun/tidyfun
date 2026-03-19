# Package index

## Tidy data wrangling

Converting, reshaping, and evaluating functional data inside data frames

- [`tidyfun`](https://tidyfun.github.io/tidyfun/reference/tidyfun-package.md)
  [`tidyfun-package`](https://tidyfun.github.io/tidyfun/reference/tidyfun-package.md)
  : tidyfun: Tidy Functional Data Wrangling and Visualization

- [`tf_gather()`](https://tidyfun.github.io/tidyfun/reference/tf_gather.md)
  :

  Gather all columns representing functional measurements into a
  `tfd`-object

- [`tf_spread()`](https://tidyfun.github.io/tidyfun/reference/tf_spread.md)
  :

  Spread a `tf`-column into many columns representing the function
  evaluations.

- [`tf_nest()`](https://tidyfun.github.io/tidyfun/reference/tf_nest.md)
  :

  Turn "long" tables into tidy data frames with `tf`-objects

- [`tf_unnest()`](https://tidyfun.github.io/tidyfun/reference/tf_unnest.md)
  :

  Turn (data frames with) `tf`-objects / list columns into "long"
  tables.

- [`tf_evaluate(`*`<data.frame>`*`)`](https://tidyfun.github.io/tidyfun/reference/tf_evaluate.data.frame.md)
  :

  Evaluate `tf`s inside a `data.frame`

## Visualization with ggplot2

[`tf_ggplot()`](https://tidyfun.github.io/tidyfun/reference/tf_ggplot.md)
and standard ggplot2 geoms for spaghetti plots, ribbons, and more.
Specialized geoms for heatmaps, boxplots, and sparklines.

- [`tf_ggplot()`](https://tidyfun.github.io/tidyfun/reference/tf_ggplot.md)
  : Create a tf-aware ggplot
- [`is_tf_ggplot()`](https://tidyfun.github.io/tidyfun/reference/is_tf_ggplot.md)
  : Check if object is a tf_ggplot
- [`` `+`( ``*`<tf_ggplot>`*`)`](https://tidyfun.github.io/tidyfun/reference/plus-.tf_ggplot.md)
  : Add layers to tf_ggplot objects
- [`print(`*`<tf_ggplot>`*`)`](https://tidyfun.github.io/tidyfun/reference/print.tf_ggplot.md)
  : Print method for tf_ggplot
- [`ggplot_build(`*`<tf_ggplot>`*`)`](https://tidyfun.github.io/tidyfun/reference/ggplot_build.tf_ggplot.md)
  : ggplot_build method for tf_ggplot
- [`parse_tf_aesthetics()`](https://tidyfun.github.io/tidyfun/reference/parse_tf_aesthetics.md)
  : Parse aesthetic mappings to separate tf and regular aesthetics

## Geoms and stats

ggplot2 layers for functional data

- [`stat_tf()`](https://tidyfun.github.io/tidyfun/reference/ggspaghetti.md)
  [`geom_spaghetti()`](https://tidyfun.github.io/tidyfun/reference/ggspaghetti.md)
  [`geom_meatballs()`](https://tidyfun.github.io/tidyfun/reference/ggspaghetti.md)
  :

  Spaghetti plots for `tf` objects

- [`stat_fboxplot()`](https://tidyfun.github.io/tidyfun/reference/ggfboxplot.md)
  [`geom_fboxplot()`](https://tidyfun.github.io/tidyfun/reference/ggfboxplot.md)
  :

  Functional boxplots for `tf` objects

- [`gglasagna()`](https://tidyfun.github.io/tidyfun/reference/gglasagna.md)
  :

  Lasagna plots for `tf`s using `ggplot2`

- [`stat_errorband()`](https://tidyfun.github.io/tidyfun/reference/ggerrorband.md)
  [`geom_errorband()`](https://tidyfun.github.io/tidyfun/reference/ggerrorband.md)
  :

  Error bands using `tf` objects as bounds

- [`stat_capellini()`](https://tidyfun.github.io/tidyfun/reference/ggcapellini.md)
  [`geom_capellini()`](https://tidyfun.github.io/tidyfun/reference/ggcapellini.md)
  :

  Glyph plots for `tf` objects

- [`autoplot(`*`<tf>`*`)`](https://tidyfun.github.io/tidyfun/reference/autoplot.tf.md)
  [`autolayer(`*`<tf>`*`)`](https://tidyfun.github.io/tidyfun/reference/autoplot.tf.md)
  :

  Autoplot and autolayer methods for `tf` objects

## Display

Pillar and print formatting for tf columns

- [`type_sum(`*`<tf>`*`)`](https://tidyfun.github.io/tidyfun/reference/tftibble.md)
  [`obj_sum(`*`<tf>`*`)`](https://tidyfun.github.io/tidyfun/reference/tftibble.md)
  [`pillar_shaft(`*`<tf>`*`)`](https://tidyfun.github.io/tidyfun/reference/tftibble.md)
  : Format tidy functional data for tibbles

## Data

Real-world functional data sets shipped with tidyfun

- [`chf_df`](https://tidyfun.github.io/tidyfun/reference/chf_df.md) :
  Congestive heart failure accelerometry data
- [`dti_df`](https://tidyfun.github.io/tidyfun/reference/dti_df.md) :
  Diffusion tensor imaging data
