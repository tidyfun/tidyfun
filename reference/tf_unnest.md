# Turn (data frames with) `tf`-objects / list columns into "long" tables.

Similar in spirit to
[`tidyr::unnest()`](https://tidyr.tidyverse.org/reference/unnest.html),
the reverse of
[`tf_nest()`](https://tidyfun.github.io/tidyfun/reference/tf_nest.md).
The `tf`-method simply turns a single `tfd` or `tfb` vector into a
"long"
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html).

## Usage

``` r
tf_unnest(data, cols, arg, interpolate = TRUE, ...)

# S3 method for class 'tf'
tf_unnest(data, cols, arg, interpolate = TRUE, ...)

# S3 method for class 'data.frame'
tf_unnest(
  data,
  cols,
  arg,
  interpolate = TRUE,
  keep_empty = FALSE,
  ptype = NULL,
  names_sep = "_",
  names_repair = "check_unique",
  ...
)
```

## Arguments

- data:

  a data.frame or a `tf`-object

- cols:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  List-columns to unnest.

  When selecting multiple columns, values from the same row will be
  recycled to their common size.

- arg:

  optional values for the `arg` argument of
  [`tf::tf_evaluate()`](https://tidyfun.github.io/tf/reference/tf_evaluate.html)

- interpolate:

  return function values for `arg`-values not on original grid? Defaults
  to `TRUE`.

- ...:

  not used currently

- keep_empty:

  By default, you get one row of output for each element of the list
  that you are unchopping/unnesting. This means that if there's a size-0
  element (like `NULL` or an empty data frame or vector), then that
  entire row will be dropped from the output. If you want to preserve
  all rows, use `keep_empty = TRUE` to replace size-0 elements with a
  single row of missing values.

- ptype:

  Optionally, a named list of column name-prototype pairs to coerce
  `cols` to, overriding the default that will be guessed from combining
  the individual values. Alternatively, a single empty ptype can be
  supplied, which will be applied to all `cols`.

- names_sep:

  If `NULL`, the default, the outer names will come from the inner
  names. If a string, the outer names will be formed by pasting together
  the outer and the inner column names, separated by `names_sep`.

- names_repair:

  Used to check that output data frame has valid names. Must be one of
  the following options:

  - `"minimal`": no name repair or checks, beyond basic existence,

  - `"unique`": make sure names are unique and not empty,

  - `"check_unique`": (the default), no name repair, but check they are
    unique,

  - `"universal`": make the names unique and syntactic

  - a function: apply custom name repair.

  - [tidyr_legacy](https://tidyr.tidyverse.org/reference/tidyr_legacy.html):
    use the name repair from tidyr 0.8.

  - a formula: a purrr-style anonymous function (see
    [`rlang::as_function()`](https://rlang.r-lib.org/reference/as_function.html))

  See
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)
  for more details on these terms and the strategies used to enforce
  them.

## Value

a "long" data frame with `tf`-columns expanded into `arg, value`-
columns.

## Details

- Caution: this uses slightly different defaults for names of unnested
  columns than
  [`tidyr::unnest()`](https://tidyr.tidyverse.org/reference/unnest.html).

- For `data.frames`, include an ID column with a unique row identifier
  before unnesting. Without it, arg-value pairs cannot be matched back
  to their original functions after unnesting.

## See also

[`tf_evaluate.data.frame()`](https://tidyfun.github.io/tidyfun/reference/tf_evaluate.data.frame.md)

Other tidyfun data wrangling functions:
[`tf_evaluate.data.frame()`](https://tidyfun.github.io/tidyfun/reference/tf_evaluate.data.frame.md),
[`tf_gather()`](https://tidyfun.github.io/tidyfun/reference/tf_gather.md),
[`tf_nest()`](https://tidyfun.github.io/tidyfun/reference/tf_nest.md),
[`tf_spread()`](https://tidyfun.github.io/tidyfun/reference/tf_spread.md)

## Examples

``` r
d <- dplyr::tibble(id = 1:3)
d$f <- tf_rgp(3, 11L)
tf_unnest(d, f)
#> # A tibble: 33 × 3
#>       id f_arg f_value
#>    <int> <dbl>   <dbl>
#>  1     1   0   -0.0291
#>  2     1   0.1  0.122 
#>  3     1   0.2  0.373 
#>  4     1   0.3  0.529 
#>  5     1   0.4  0.444 
#>  6     1   0.5  0.116 
#>  7     1   0.6 -0.487 
#>  8     1   0.7 -1.14  
#>  9     1   0.8 -1.43  
#> 10     1   0.9 -1.29  
#> # ℹ 23 more rows
```
