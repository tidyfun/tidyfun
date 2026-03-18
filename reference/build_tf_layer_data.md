# Build long-format data and new mapping for a single tf layer

Evaluates the layer's tf aesthetics independently on their own natural
grid (or the user-specified grid). Called by
[`finalize_tf_ggplot()`](https://tidyfun.github.io/tidyfun/reference/finalize_tf_ggplot.md)
per layer.

## Usage

``` r
build_tf_layer_data(
  layer_info,
  plot_tf_aes,
  scalar_col_map,
  layer_idx,
  enriched_data,
  user_arg,
  interpolate
)
```

## Arguments

- layer_info:

  List from `all_layers` with `layer`, `parsed_aes`, `is_tf_layer`

- plot_tf_aes:

  tf aesthetics from the plot level (used when `inherit.aes = TRUE`)

- scalar_col_map:

  Named list: key -\> column name in enriched_data

- layer_idx:

  Integer index of this layer (for keying layer-level scalar aes)

- enriched_data:

  Data frame with original data + pre-evaluated scalar tf columns

- user_arg:

  Optional evaluation grid (overrides natural grid)

- interpolate:

  Whether to interpolate tf objects to `arg`

## Value

`NULL` when no effective tf aes, otherwise
`list(long_data, new_mapping)`
