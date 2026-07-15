# Build long-format data and mapping for a single multivariate (tf_mv) aesthetic

Two displays, mirroring
[`tf::plot.tf_mv()`](https://tidyfun.github.io/tf/reference/plot.tf_mv.html):
`"trajectory"` (the planar curve x(t) vs y(t), default for `d == 2`) and
`"facet"` (value-vs-arg, one group per curve x component, default
otherwise). The caller (a `tf_mv` aesthetic mapped via `aes(tf = ...)`)
guarantees this is the only tf aesthetic in the layer.

## Usage

``` r
build_tf_mv_layer_data(
  mv,
  mv_quo,
  geom,
  parsed_aes,
  scalar_col_map,
  layer_idx,
  enriched_data,
  user_arg,
  interpolate,
  mv_type = NULL
)
```

## Arguments

- mv:

  The evaluated `tf_mv` object.

- mv_quo:

  The quosure for the aesthetic (used for axis labels / source column).

- geom:

  The layer geom, used to reject geoms that reorder trajectories.

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

- mv_type:

  `"trajectory"`, `"facet"`, or `NULL` (resolve from `d`).

## Value

`list(long_data, new_mapping, axis_labels)`.
