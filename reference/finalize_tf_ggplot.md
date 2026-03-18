# Finalize tf_ggplot by processing all tf layers independently

Each layer is evaluated on its own natural argument grid (Option B
architecture). The base ggplot has `data = NULL`; each layer receives
its own long-format data.

## Usage

``` r
finalize_tf_ggplot(tf_plot)
```

## Arguments

- tf_plot:

  A tf_ggplot object with accumulated layers

## Value

Regular ggplot object with all layers properly transformed
