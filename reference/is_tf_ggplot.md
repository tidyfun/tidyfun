# Check if object is a tf_ggplot

Check if object is a tf_ggplot

## Usage

``` r
is_tf_ggplot(x)
```

## Arguments

- x:

  Object to test

## Value

`TRUE` if `x` inherits from `"tf_ggplot"`, `FALSE` otherwise.

## Examples

``` r
p <- tf_ggplot(data.frame(x = 1))
is_tf_ggplot(p)
#> [1] TRUE
is_tf_ggplot(ggplot2::ggplot())
#> [1] FALSE
```
