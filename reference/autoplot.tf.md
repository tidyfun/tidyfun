# Autoplot and autolayer methods for `tf` objects

Convenient plotting methods for `tf` objects.
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
creates a complete spaghetti plot,
[`autolayer()`](https://ggplot2.tidyverse.org/reference/autolayer.html)
creates a layer that can be added to an existing
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
or
[`tf_ggplot()`](https://tidyfun.github.io/tidyfun/reference/tf_ggplot.md).

## Usage

``` r
# S3 method for class 'tf'
autoplot(object, ...)

# S3 method for class 'tf'
autolayer(object, ...)
```

## Arguments

- object:

  a `tf` object

- ...:

  passed to
  [`ggplot2::geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html)

## Value

A
[`tf_ggplot()`](https://tidyfun.github.io/tidyfun/reference/tf_ggplot.md)
object for
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html), a
[`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
object for
[`autolayer()`](https://ggplot2.tidyverse.org/reference/autolayer.html).

## See also

Other tidyfun visualization:
[`ggcapellini`](https://tidyfun.github.io/tidyfun/reference/ggcapellini.md),
[`gglasagna()`](https://tidyfun.github.io/tidyfun/reference/gglasagna.md),
[`ggspaghetti`](https://tidyfun.github.io/tidyfun/reference/ggspaghetti.md)

## Examples

``` r
# \donttest{
library(ggplot2)
f <- tf_rgp(5)
autoplot(f)

ggplot() + autolayer(f)

tf_ggplot() + autolayer(f)
#> ℹ `geom_spaghetti()` layer automatically translated for `tf_ggplot()`
#> • Use `geom_line()` with `aes(tf = f)` directly to silence this
#> This message is displayed once every 8 hours.

# }
```
