# Turn "long" tables into tidy data frames with `tf`-objects

Similar in spirit to
[`tidyr::nest()`](https://tidyr.tidyverse.org/reference/nest.html). This
turns tables in "long" format, where one column (`.id`) defines the unit
of observation, one column (`.arg`) defines the evaluation grids of the
functional observations, and other columns (`...`) define the values of
the functions at those points into a (much shorter) table containing
`tfd`-objects. All other variables are checked for constancy over `.id`
and appended as well.

## Usage

``` r
tf_nest(
  data,
  ...,
  .id = "id",
  .arg = "arg",
  domain = NULL,
  evaluator = "tf_approx_linear"
)
```

## Arguments

- data:

  a data frame

- ...:

  A selection of columns. If empty, all variables except the `.id` and
  `.arg` columns are selected. You can supply bare variable names,
  select all variables between `x` and `z` with `x:z`, exclude `y` with
  `-y`. For more options, see the
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  documentation.

- .id:

  the (bare or quoted) name of the column defining the different
  observations. Defaults to "id".

- .arg:

  the (bare or quoted) name of the column defining the `arg`-values of
  the observed functions. Defaults to "arg".

- domain:

  optional. Range of possible `arg`-values. See
  [`tf::tfd()`](https://tidyfun.github.io/tf/reference/tfd.html) for
  details.

- evaluator:

  optional. A function accepting arguments x, arg, evaluations. See
  [`tf::tfd()`](https://tidyfun.github.io/tf/reference/tfd.html) for
  details.

## Value

a data frame with (at least) `.id` and `tfd` columns

## Details

`domain` and `evaluator` can be specified as lists or vectors if you are
nesting multiple functional data columns with different properties.
Because this interface captures evaluator names as text, supply the
evaluator as a string rather than a bare function name.

## See also

[`tf::tfd()`](https://tidyfun.github.io/tf/reference/tfd.html) for
details on `domain` and `evaluator`.

Other tidyfun data wrangling functions:
[`tf_evaluate.data.frame()`](https://tidyfun.github.io/tidyfun/reference/tf_evaluate.data.frame.md),
[`tf_gather()`](https://tidyfun.github.io/tidyfun/reference/tf_gather.md),
[`tf_spread()`](https://tidyfun.github.io/tidyfun/reference/tf_spread.md),
[`tf_unnest()`](https://tidyfun.github.io/tidyfun/reference/tf_unnest.md)

## Examples

``` r
d <- dplyr::tibble(id = rep(1:3, each = 5), arg = rep(1:5, 3), value = rnorm(15))
tf_nest(d, .id = id, .arg = arg)
#> # A tibble: 3 × 2
#>      id     value
#>   <int> <tfd_reg>
#> 1     1     ▄▂▁█▇
#> 2     2     ▄▄▃█▅
#> 3     3     ▃▂▅▆█
```
