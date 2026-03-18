# Evaluate `tf`s inside a `data.frame`

Evaluate `tf`s inside a `data.frame`

## Usage

``` r
# S3 method for class 'data.frame'
tf_evaluate(object, ..., arg)
```

## Arguments

- object:

  a `data.frame`-like object with `tf` columns.

- ...:

  optional: a selection of `tf`-columns. If empty, all `tf`-variables in
  the data frame are selected. You can supply bare variable names,
  select all variables between `x` and `z` with `x:z`, exclude `y` with
  `-y`. For more options, see the
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  documentation.

- arg:

  optional evaluation grid (vector or list of vectors). Defaults to
  `tf_arg(object)`.

## Value

Replaces `tf`-columns with list columns of smaller `data.frames`
containing the functions' arguments (`arg`) and evaluations (`value`)
and returns the modified nested dataframe.

## Details

The `arg`-argument of `tf_evaluate.data.frame` method can be a list of
`arg`-vectors or -lists used as the `arg` argument for the
[`tf::tf_evaluate()`](https://tidyfun.github.io/tf/reference/tf_evaluate.html)-method
for the respective `tf`-columns in `object`. `...` is not used for a
`tf`-`object`, but a second unnamed argument to these methods will be
interpreted as `arg`.

## See also

Other tidyfun data wrangling functions:
[`tf_gather()`](https://tidyfun.github.io/tidyfun/reference/tf_gather.md),
[`tf_nest()`](https://tidyfun.github.io/tidyfun/reference/tf_nest.md),
[`tf_spread()`](https://tidyfun.github.io/tidyfun/reference/tf_spread.md),
[`tf_unnest()`](https://tidyfun.github.io/tidyfun/reference/tf_unnest.md)

## Examples

``` r
d <- dplyr::tibble(id = 1:3)
d$f <- tf_rgp(3, 11L)
str(tf_evaluate(d))
#> tibble [3 × 2] (S3: tbl_df/tbl/data.frame)
#>  $ id: int [1:3] 1 2 3
#>  $ f :List of 3
#>   ..$ 1:'data.frame':    11 obs. of  2 variables:
#>   .. ..$ arg  : num [1:11] 0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 ...
#>   .. ..$ value: num [1:11] 1.372 1.509 1.301 0.705 0.216 ...
#>   ..$ 2:'data.frame':    11 obs. of  2 variables:
#>   .. ..$ arg  : num [1:11] 0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 ...
#>   .. ..$ value: num [1:11] 0.352 0.432 -0.216 -1.086 -1.545 ...
#>   ..$ 3:'data.frame':    11 obs. of  2 variables:
#>   .. ..$ arg  : num [1:11] 0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 ...
#>   .. ..$ value: num [1:11] -1.054 -1.062 -0.899 -0.646 -0.322 ...
```
