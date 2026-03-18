# Spread a `tf`-column into many columns representing the function evaluations.

Similar in spirit to
[`tidyr::spread()`](https://tidyr.tidyverse.org/reference/spread.html),
but does NOT shorten, just widens the data frame – a `tf`-column is
spread out into many columns containing the functional measurements.

## Usage

``` r
tf_spread(data, value, arg, sep = "_", interpolate = FALSE)
```

## Arguments

- data:

  a data frame with at least one `tf`-column

- value:

  the name of the `tf`-column to 'spread'/evaluate. You can supply bare
  variable names etc., see the
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  documentation. Also works without this if there's only one `tf` in
  `data`, see examples.

- arg:

  (Semi-)optional. A vector of `arg`-values on which to evaluate the
  functions. If not provided, uses the default `arg`s. Should be
  specified for `tf_irreg`, otherwise *all* observed gridpoints are used
  for *every* function.

- sep:

  separating character used to create column names for the new columns,
  defaults to `"_"` for column names "\<name of the
  `tf`\>\_\<`arg`-value\>". Set to NULL to get column names that only
  contain the `arg`-value.

- interpolate:

  `interpolate`-argument for evaluating the functional data. Defaults to
  FALSE, i.e., `tfd`s are *not* inter/extrapolated on unobserved
  `arg`-values.

## Value

a wider dataframe with the `tf`-column spread out into many columns each
containing the functional measurements for one `arg`-value.

## See also

Other tidyfun data wrangling functions:
[`tf_evaluate.data.frame()`](https://tidyfun.github.io/tidyfun/reference/tf_evaluate.data.frame.md),
[`tf_gather()`](https://tidyfun.github.io/tidyfun/reference/tf_gather.md),
[`tf_nest()`](https://tidyfun.github.io/tidyfun/reference/tf_nest.md),
[`tf_unnest()`](https://tidyfun.github.io/tidyfun/reference/tf_unnest.md)

## Examples

``` r
d <- dplyr::tibble(g = 1:3)
d$f <- tf_rgp(3, 11L)
tf_spread(d, f)
#> # A tibble: 3 × 12
#>       g    f_0  f_0.1  f_0.2     f_0.3   f_0.4   f_0.5   f_0.6  f_0.7  f_0.8
#> * <int>  <dbl>  <dbl>  <dbl>     <dbl>   <dbl>   <dbl>   <dbl>  <dbl>  <dbl>
#> 1     1 -1.25  -1.39  -1.11  -0.584    -0.225  -0.0513  0.0368  0.130  0.522
#> 2     2  1.21   0.467  0.132  0.000343 -0.276  -0.796  -1.06   -0.979 -0.605
#> 3     3 -0.609 -0.386 -0.213 -0.103    -0.0379 -0.262  -0.544  -0.784 -0.628
#> # ℹ 2 more variables: f_0.9 <dbl>, f_1 <dbl>
tf_spread(d, -g)
#> # A tibble: 3 × 12
#>       g    f_0  f_0.1  f_0.2     f_0.3   f_0.4   f_0.5   f_0.6  f_0.7  f_0.8
#> * <int>  <dbl>  <dbl>  <dbl>     <dbl>   <dbl>   <dbl>   <dbl>  <dbl>  <dbl>
#> 1     1 -1.25  -1.39  -1.11  -0.584    -0.225  -0.0513  0.0368  0.130  0.522
#> 2     2  1.21   0.467  0.132  0.000343 -0.276  -0.796  -1.06   -0.979 -0.605
#> 3     3 -0.609 -0.386 -0.213 -0.103    -0.0379 -0.262  -0.544  -0.784 -0.628
#> # ℹ 2 more variables: f_0.9 <dbl>, f_1 <dbl>
tf_spread(d)
#> # A tibble: 3 × 12
#>       g    f_0  f_0.1  f_0.2     f_0.3   f_0.4   f_0.5   f_0.6  f_0.7  f_0.8
#> * <int>  <dbl>  <dbl>  <dbl>     <dbl>   <dbl>   <dbl>   <dbl>  <dbl>  <dbl>
#> 1     1 -1.25  -1.39  -1.11  -0.584    -0.225  -0.0513  0.0368  0.130  0.522
#> 2     2  1.21   0.467  0.132  0.000343 -0.276  -0.796  -1.06   -0.979 -0.605
#> 3     3 -0.609 -0.386 -0.213 -0.103    -0.0379 -0.262  -0.544  -0.784 -0.628
#> # ℹ 2 more variables: f_0.9 <dbl>, f_1 <dbl>
```
