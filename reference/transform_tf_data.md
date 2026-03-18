# Transform data with tf aesthetics into long format

Transform data with tf aesthetics into long format

## Usage

``` r
transform_tf_data(data, tf_aesthetics, arg = NULL, interpolate = TRUE)
```

## Arguments

- data:

  Data frame containing tf objects.

- tf_aesthetics:

  List of tf aesthetic mappings.

- arg:

  Optional evaluation grid.

- interpolate:

  Whether to interpolate tf objects.

## Value

A data frame in long format with tf columns expanded.

## Examples

``` r
d <- data.frame(g = 1:3)
d$f <- tf_rgp(3)
tf_aes <- parse_tf_aesthetics(ggplot2::aes(tf = f))$tf_aes
head(transform_tf_data(d, tf_aes))
#> Warning: Large data expansion: Plotting data expanded to 153 rows (from 3 rows - factor
#> of 51)
#> ℹ This may impact memory usage and plotting performance
#> ℹ Use `arg` to specify a coarser evaluation grid
#> # A tibble: 6 × 4
#>    f.id f.arg f.value     g
#>   <dbl> <dbl>   <dbl> <int>
#> 1     1  0      1.09      1
#> 2     1  0.02   1.06      1
#> 3     1  0.04   1.00      1
#> 4     1  0.06   0.889     1
#> 5     1  0.08   0.797     1
#> 6     1  0.1    0.641     1
```
