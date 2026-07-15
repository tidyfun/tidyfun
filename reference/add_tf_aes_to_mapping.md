# Add a tf aesthetic to an ggplot2 mapping object

Add a tf aesthetic to an ggplot2 mapping object

## Usage

``` r
add_tf_aes_to_mapping(
  mapping,
  aes_name,
  val_col,
  arg_col,
  id_col,
  planar_xy = FALSE
)
```

## Arguments

- planar_xy:

  `TRUE` when both `tf_x` and `tf_y` are present in the same layer (a
  planar curve x(t) vs y(t)). In that case `tf_y` must NOT overwrite `x`
  with its arg grid – `x` comes from `tf_x`'s function values.
