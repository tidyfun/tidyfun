# Autoplot and autolayer methods for multivariate (`tf_mv`) objects

Plotting methods for vector-valued functional data (`tf_mv`, functions
\\R \to R^d\\), mirroring the display modes of
[`tf::plot.tf_mv()`](https://tidyfun.github.io/tf/reference/plot.tf_mv.html).

## Usage

``` r
# S3 method for class 'tf_mv'
autoplot(object, ..., type = NULL, colour_by_arg = FALSE)

# S3 method for class 'tf_mv'
autolayer(object, ..., type = NULL, colour_by_arg = FALSE)
```

## Arguments

- object:

  a `tf_mv` object

- ...:

  passed to
  [`geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
  (trajectory) or
  [`geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
  (facet)

- type:

  `"trajectory"`, `"facet"`, or `NULL` to resolve from the number of
  components (see Details).

- colour_by_arg:

  colour the segments of trajectories by their argument value? Defaults
  to `FALSE`. Only available for `type = "trajectory"`.

## Value

A
[`tf_ggplot()`](https://tidyfun.github.io/tidyfun/reference/tf_ggplot.md)
object for
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html), a
[`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
for
[`autolayer()`](https://ggplot2.tidyverse.org/reference/autolayer.html).

## Details

For two-component objects (`d == 2`) the default is a `"trajectory"`
plot of the planar curve x(t) vs y(t) (drawn with
[`geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html),
which connects in argument order). Otherwise the default is `"facet"`:
value-vs-arg curves with one panel per output dimension.
[`autolayer()`](https://ggplot2.tidyverse.org/reference/autolayer.html)
returns a single layer (no faceting) and works with plain
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
as well as
[`tf_ggplot()`](https://tidyfun.github.io/tidyfun/reference/tf_ggplot.md).

Trajectory plots lose the information about *where* along the domain a
point lies. Set `colour_by_arg = TRUE` to colour each segment of the
trajectories by its argument value. This maps the `.arg` column that
[`tf_ggplot()`](https://tidyfun.github.io/tidyfun/reference/tf_ggplot.md)
provides to the `colour` aesthetic, so
`autoplot(mv, colour_by_arg = TRUE)` is equivalent to
`tf_ggplot(data, aes(tf = mv, colour = .arg)) + geom_path()`.

## See also

[`tf_phaseplane()`](https://tidyfun.github.io/tidyfun/reference/tf_phaseplane.md)
for phase-plane plots of univariate functions.

Other tidyfun visualization:
[`autoplot.tf()`](https://tidyfun.github.io/tidyfun/reference/autoplot.tf.md),
[`ggcapellini`](https://tidyfun.github.io/tidyfun/reference/ggcapellini.md),
[`gglasagna()`](https://tidyfun.github.io/tidyfun/reference/gglasagna.md),
[`ggspaghetti`](https://tidyfun.github.io/tidyfun/reference/ggspaghetti.md),
[`tf_phaseplane()`](https://tidyfun.github.io/tidyfun/reference/tf_phaseplane.md)

## Examples

``` r
library(ggplot2)
mv <- tfd_mv(list(x = tf_rgp(5), y = tf_rgp(5)))
autoplot(mv)

autoplot(mv, colour_by_arg = TRUE)

ggplot() + autolayer(mv)

# phase-plane plot of a univariate function (velocity vs. acceleration):
f <- tf_rgp(3, arg = 101L, nugget = 0)
autoplot(tf_phaseplane(f), colour_by_arg = TRUE)
```
