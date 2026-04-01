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
d <- tf_spread(growth[1:5, ]) |> dplyr::mutate(id = 1:dplyr::n())
dplyr::glimpse(d)
#> Rows: 5
#> Columns: 33
#> $ gender      <fct> female, female, female, female, female
#> $ height_1    <dbl> 76.2, 74.6, 78.2, 77.7, 76.0
#> $ height_1.25 <dbl> 80.4, 78.0, 81.8, 80.5, 80.0
#> $ height_1.5  <dbl> 83.3, 82.0, 85.4, 83.3, 83.0
#> $ height_1.75 <dbl> 85.7, 86.9, 87.9, 87.0, 86.0
#> $ height_2    <dbl> 87.7, 90.0, 89.6, 90.3, 89.0
#> $ height_3    <dbl> 96.0, 94.9, 97.1, 98.6, 96.0
#> $ height_4    <dbl> 103.8, 102.1, 109.2, 106.3, 103.0
#> $ height_5    <dbl> 110.7, 109.2, 116.2, 113.9, 110.0
#> $ height_6    <dbl> 116.8, 115.6, 122.9, 120.7, 117.0
#> $ height_7    <dbl> 122.2, 122.7, 128.0, 125.7, 123.0
#> $ height_8    <dbl> 127.4, 128.2, 134.2, 131.0, 131.0
#> $ height_8.5  <dbl> 130.6, 131.1, 138.0, 134.1, 133.0
#> $ height_9    <dbl> 133.4, 134.8, 141.5, 137.1, 136.0
#> $ height_9.5  <dbl> 135.9, 138.2, 145.2, 140.2, 139.0
#> $ height_10   <dbl> 138.6, 140.9, 148.8, 143.0, 141.0
#> $ height_10.5 <dbl> 142.4, 143.4, 152.3, 145.7, 144.0
#> $ height_11   <dbl> 146.8, 146.1, 155.6, 148.5, 147.0
#> $ height_11.5 <dbl> 150.3, 149.4, 158.2, 151.5, 150.0
#> $ height_12   <dbl> 153.1, 152.9, 159.6, 154.8, 153.0
#> $ height_12.5 <dbl> 155.0, 156.4, 160.1, 158.4, 157.0
#> $ height_13   <dbl> 156.2, 159.5, 160.3, 161.2, 161.0
#> $ height_13.5 <dbl> 157.1, 161.6, 160.8, 163.5, 164.0
#> $ height_14   <dbl> 157.7, 162.6, 161.6, 165.2, 166.0
#> $ height_14.5 <dbl> 158.0, 163.6, 161.8, 166.0, 167.0
#> $ height_15   <dbl> 158.2, 165.0, 161.7, 166.5, 168.0
#> $ height_15.5 <dbl> 158.4, 165.3, 161.8, 166.9, 168.0
#> $ height_16   <dbl> 158.6, 165.6, 161.9, 167.2, 169.0
#> $ height_16.5 <dbl> 158.7, 165.9, 161.9, 167.4, 169.0
#> $ height_17   <dbl> 158.7, 166.1, 161.7, 167.4, 170.0
#> $ height_17.5 <dbl> 158.8, 166.0, 161.9, 167.6, 170.0
#> $ height_18   <dbl> 158.9, 166.0, 162.2, 167.8, 170.0
#> $ id          <int> 1, 2, 3, 4, 5
# tidyselect syntax for column selection:
tf_gather(d, starts_with("height"))
#> creating new <tfd>-column "height"
#>        gender id                     height
#> girl01 female  1 ▁▁▁▂▃▃▄▄▅▅▆▆▆▆▇▇▇▇▇▇██████
#> girl02 female  2 ▁▁▁▂▃▃▄▄▅▅▆▆▆▆▇▇▇█████████
#> girl03 female  3 ▁▁▂▂▃▄▄▅▅▆▆▇▇▇▇███████████
#> girl04 female  4 ▁▁▁▂▃▃▄▅▅▅▆▆▆▇▇▇██████████
#> girl05 female  5 ▁▁▁▂▃▃▄▄▅▅▆▆▆▆▇▇██████████
tf_gather(d, height_1:height_18)
#> creating new <tfd>-column "height"
#>        gender id                     height
#> girl01 female  1 ▁▁▁▂▃▃▄▄▅▅▆▆▆▆▇▇▇▇▇▇██████
#> girl02 female  2 ▁▁▁▂▃▃▄▄▅▅▆▆▆▆▇▇▇█████████
#> girl03 female  3 ▁▁▂▂▃▄▄▅▅▆▆▇▇▇▇███████████
#> girl04 female  4 ▁▁▁▂▃▃▄▅▅▅▆▆▆▇▇▇██████████
#> girl05 female  5 ▁▁▁▂▃▃▄▄▅▅▆▆▆▆▇▇██████████
tf_gather(d, -gender, -id)
#> creating new <tfd>-column "height"
#>        gender id                     height
#> girl01 female  1 ▁▁▁▂▃▃▄▄▅▅▆▆▆▆▇▇▇▇▇▇██████
#> girl02 female  2 ▁▁▁▂▃▃▄▄▅▅▆▆▆▆▇▇▇█████████
#> girl03 female  3 ▁▁▂▂▃▄▄▅▅▆▆▇▇▇▇███████████
#> girl04 female  4 ▁▁▁▂▃▃▄▅▅▅▆▆▆▇▇▇██████████
#> girl05 female  5 ▁▁▁▂▃▃▄▄▅▅▆▆▆▆▇▇██████████
# custom key name and arg values:
tf_gather(d, starts_with("height"), key = "height")
#>        gender id                     height
#> girl01 female  1 ▁▁▁▂▃▃▄▄▅▅▆▆▆▆▇▇▇▇▇▇██████
#> girl02 female  2 ▁▁▁▂▃▃▄▄▅▅▆▆▆▆▇▇▇█████████
#> girl03 female  3 ▁▁▂▂▃▄▄▅▅▆▆▇▇▇▇███████████
#> girl04 female  4 ▁▁▁▂▃▃▄▅▅▅▆▆▆▇▇▇██████████
#> girl05 female  5 ▁▁▁▂▃▃▄▄▅▅▆▆▆▆▇▇██████████
tf_gather(d, starts_with("height"), arg = seq(0, 1, length.out = 31))
#> creating new <tfd>-column "height"
#>        gender id                     height
#> girl01 female  1 ▁▁▁▂▃▃▄▄▅▅▆▆▆▆▇▇▇▇▇▇██████
#> girl02 female  2 ▁▁▁▂▃▃▄▄▅▅▆▆▆▆▇▇▇█████████
#> girl03 female  3 ▁▁▂▂▃▄▄▅▅▆▆▇▇▇▇███████████
#> girl04 female  4 ▁▁▁▂▃▃▄▅▅▅▆▆▆▇▇▇██████████
#> girl05 female  5 ▁▁▁▂▃▃▄▄▅▅▆▆▆▆▇▇██████████
```
