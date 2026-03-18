# Translate a geom_spaghetti/geom_meatballs/geom_errorband layer for tf_ggplot

Remaps old-style y/ymin/ymax tf aesthetics to tf/tf_ymin/tf_ymax and
substitutes the appropriate standard ggplot2 geom.

## Usage

``` r
translate_old_tf_layer(layer, e1)
```

## Arguments

- layer:

  A LayerInstance using StatTf or StatErrorband

- e1:

  The tf_ggplot object (may be modified to clean up plot-level y)

## Value

A list with `plot_obj` (possibly modified e1) and `layers` (list of new
layers to add)
