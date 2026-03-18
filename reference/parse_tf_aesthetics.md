# Parse aesthetic mappings to separate tf and regular aesthetics

Parse aesthetic mappings to separate tf and regular aesthetics

## Usage

``` r
parse_tf_aesthetics(mapping, data = NULL)
```

## Arguments

- mapping:

  An aesthetic mapping created with
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).

- data:

  Data frame to evaluate expressions against.

## Value

A list with components `tf_aes`, `scalar_tf_aes`, and `regular_aes`.

## Examples

``` r
parse_tf_aesthetics(ggplot2::aes(tf = f, color = group))
#> $tf_aes
#> Aesthetic mapping: 
#> * `tf` -> `f`
#> 
#> $scalar_tf_aes
#> Aesthetic mapping: 
#> <empty>
#> 
#> $regular_aes
#> $colour
#> <quosure>
#> expr: ^group
#> env:  0x561d7d1471b8
#> 
#> attr(,"class")
#> [1] "uneval"
#> attr(,"S7_class")
#> <ggplot2::mapping> class
#> @ parent     : S3<gg>
#> @ constructor: function(x, ..., env) {...}
#> @ validator  : <NULL>
#> @ properties :
#> 
parse_tf_aesthetics(ggplot2::aes(x = x, y = y))
#> $tf_aes
#> Aesthetic mapping: 
#> <empty>
#> 
#> $scalar_tf_aes
#> Aesthetic mapping: 
#> <empty>
#> 
#> $regular_aes
#> $x
#> <quosure>
#> expr: ^x
#> env:  0x561d7d1471b8
#> 
#> $y
#> <quosure>
#> expr: ^y
#> env:  0x561d7d1471b8
#> 
#> attr(,"class")
#> [1] "uneval"
#> attr(,"S7_class")
#> <ggplot2::mapping> class
#> @ parent     : S3<gg>
#> @ constructor: function(x, ..., env) {...}
#> @ validator  : <NULL>
#> @ properties :
#> 
```
