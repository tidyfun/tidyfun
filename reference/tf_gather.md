# Gather all columns representing functional measurements into a `tfd`-object

Similar in spirit to
[`tidyr::gather()`](https://tidyr.tidyverse.org/reference/gather.html),
but does NOT put the values in the gathered columns into one very long
"value"-column while labeling the different original columns in a very
long "key"-column – instead it creates a `tfd`-column containing the
functional measurements of the columns given in `...`.

## Usage

``` r
tf_gather(
  data,
  ...,
  key = ".tfd",
  arg = NULL,
  domain = NULL,
  evaluator = tf_approx_linear
)
```

## Arguments

- data:

  a data frame – note that `dplyr` does not handle matrix columns well,
  if `data` contains more than one of those, `tf_gather` will fail...

- ...:

  A selection of columns to collect as a `tfd` object. Each column
  represents measurements of a functional variable at a specific
  `arg`-val. Can also be the name of a matrix-valued column, but see
  above. If empty, all variables are selected. You can supply bare
  variable names, select all variables between x and z with x:z, exclude
  y with -y. For more options, see the
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  documentation.

- key:

  the name of the created `tfd`-column. Defaults to `".tfd"`, but the
  function will try to guess the name based on the column names of the
  gathered columns in this case. If a common prefix of all column names
  is found, this is used instead. You also get a message about this.

- arg:

  optional. Argument values for the functions. If not provided, will be
  guessed from the column names as well. See also
  [`tf::tfd()`](https://tidyfun.github.io/tf/reference/tfd.html).

- domain:

  optional. Range of possible `arg`-values. See
  [`tf::tfd()`](https://tidyfun.github.io/tf/reference/tfd.html) for
  details.

- evaluator:

  optional. A function accepting arguments x, arg, evaluations. See
  [`tf::tfd()`](https://tidyfun.github.io/tf/reference/tfd.html) for
  details.

## Value

a modified `data.frame` with a `tfd` column replacing the `...`.

## See also

[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)

Other tidyfun data wrangling functions:
[`tf_evaluate.data.frame()`](https://tidyfun.github.io/tidyfun/reference/tf_evaluate.data.frame.md),
[`tf_nest()`](https://tidyfun.github.io/tidyfun/reference/tf_nest.md),
[`tf_spread()`](https://tidyfun.github.io/tidyfun/reference/tf_spread.md),
[`tf_unnest()`](https://tidyfun.github.io/tidyfun/reference/tf_unnest.md)

## Examples

``` r
data(growth, package = "tf")
(d <- tibble::as_tibble(as.data.frame(growth[1:5, ]$height[, 1:10, matrix = TRUE])))
#> # A tibble: 5 × 10
#>     `1`   `2`   `3`   `4`   `5`   `6`   `7`   `8`   `9`  `10`
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  76.2  87.7  96    104.  111.  117.  122.  127.  133.  139.
#> 2  74.6  90    94.9  102.  109.  116.  123.  128.  135.  141.
#> 3  78.2  89.6  97.1  109.  116.  123.  128   134.  142.  149.
#> 4  77.7  90.3  98.6  106.  114.  121.  126.  131   137.  143 
#> 5  76    89    96    103   110   117   123   131   136   141 
tf_gather(d)
#> # A tibble: 5 × 1
#>         .tfd
#>    <tfd_reg>
#> 1 ▁▂▃▄▄▅▆▆▇▇
#> 2 ▁▂▃▃▄▅▆▆▇█
#> 3 ▁▂▃▄▅▆▆▇██
#> 4 ▁▂▃▄▅▅▆▇▇█
#> 5 ▁▂▃▄▄▅▆▇▇█
tf_gather(d, key = "height_tf")
#> # A tibble: 5 × 1
#>    height_tf
#>    <tfd_reg>
#> 1 ▁▂▃▄▄▅▆▆▇▇
#> 2 ▁▂▃▃▄▅▆▆▇█
#> 3 ▁▂▃▄▅▆▆▇██
#> 4 ▁▂▃▄▅▅▆▇▇█
#> 5 ▁▂▃▄▄▅▆▇▇█
tf_gather(d, arg = seq(0, 1, length.out = 10))
#> # A tibble: 5 × 1
#>         .tfd
#>    <tfd_reg>
#> 1 ▁▂▃▄▄▅▆▆▇▇
#> 2 ▁▂▃▃▄▅▆▆▇█
#> 3 ▁▂▃▄▅▆▆▇██
#> 4 ▁▂▃▄▅▅▆▇▇█
#> 5 ▁▂▃▄▄▅▆▇▇█
(d2 <- dplyr::bind_cols(id = rownames(d), d))
#> # A tibble: 5 × 11
#>   id      `1`   `2`   `3`   `4`   `5`   `6`   `7`   `8`   `9`  `10`
#>   <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 1      76.2  87.7  96    104.  111.  117.  122.  127.  133.  139.
#> 2 2      74.6  90    94.9  102.  109.  116.  123.  128.  135.  141.
#> 3 3      78.2  89.6  97.1  109.  116.  123.  128   134.  142.  149.
#> 4 4      77.7  90.3  98.6  106.  114.  121.  126.  131   137.  143 
#> 5 5      76    89    96    103   110   117   123   131   136   141 
tf_gather(d2, -id) # tf_gather(d2, matches("height")); tf_gather(d2, -1); etc
#> # A tibble: 5 × 2
#>   id          .tfd
#>   <chr>  <tfd_reg>
#> 1 1     ▁▂▃▄▄▅▆▆▇▇
#> 2 2     ▁▂▃▃▄▅▆▆▇█
#> 3 3     ▁▂▃▄▅▆▆▇██
#> 4 4     ▁▂▃▄▅▅▆▇▇█
#> 5 5     ▁▂▃▄▄▅▆▇▇█
```
