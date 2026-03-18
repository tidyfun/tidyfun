# Glyph plots for `tf` objects

Plots a miniature glyph / sparkline for each entry of a `tf`-object.
(Capellini are tiny spaghetti – *angel hair* pasta.) Aesthetics `x` and
`y` specify the location of the glyphs, the `tf` aesthetic defines their
shapes. To accommodate all my fellow idiots, `geom_cappelini`,
`geom_cappellini` and `geom_capelini` also work.

## Usage

``` r
stat_capellini(
  mapping = NULL,
  data = NULL,
  geom = "capellini",
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  arg = NULL,
  add_lines = FALSE,
  add_boxes = TRUE,
  width = NULL,
  height = NULL,
  ...
)

geom_capellini(
  mapping = NULL,
  data = NULL,
  stat = "capellini",
  position = "identity",
  ...,
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  arg = NULL,
  add_lines = TRUE,
  add_boxes = TRUE,
  width = NULL,
  height = NULL,
  box.colour = "#0000001A",
  box.linetype = 1,
  box.fill = NA,
  box.linewidth = 0.1,
  box.alpha = 0.1,
  line.colour = "black",
  line.linetype = 2,
  line.linewidth = 0.3,
  line.alpha = 0.5
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- geom:

  The geometric object to use to display the data for this layer. When
  using a `stat_*()` function to construct a layer, the `geom` argument
  can be used to override the default coupling between stats and geoms.
  The `geom` argument accepts the following:

  - A `Geom` ggproto subclass, for example `GeomPoint`.

  - A string naming the geom. To give the geom as a string, strip the
    function name of the `geom_` prefix. For example, to use
    [`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html),
    give the geom as `"point"`.

  - For more information and other ways to specify the geom, see the
    [layer
    geom](https://ggplot2.tidyverse.org/reference/layer_geoms.html)
    documentation.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- na.rm:

  remove NAs? defaults to `TRUE`

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

- arg:

  where to evaluate `tf` – defaults to the default ;)

- add_lines:

  should a reference line in the middle of the range of the functions'
  values be added to each glyph? defaults to TRUE

- add_boxes:

  should a box be added to frame each glyph? defaults to TRUE

- width:

  the width of the glyphs. Defaults to 2/3 of the
  [`ggplot2::resolution()`](https://ggplot2.tidyverse.org/reference/resolution.html)
  of the variable for the `x`-aesthetic, this will be too small if any
  values are close together.

- height:

  the height of the glyphs. Defaults to 2/3 of the
  [`ggplot2::resolution()`](https://ggplot2.tidyverse.org/reference/resolution.html)
  of the variable for the `y`-aesthetic, this will be too small if any
  values are close together.

- ...:

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html)'s
  `params` argument. These arguments broadly fall into one of 4
  categories below. Notably, further arguments to the `position`
  argument, or aesthetics that are required can *not* be passed through
  `...`. Unknown arguments that are not part of the 4 categories below
  are ignored.

  - Static aesthetics that are not mapped to a scale, but are at a fixed
    value and apply to the layer as a whole. For example,
    `colour = "red"` or `linewidth = 3`. The geom's documentation has an
    **Aesthetics** section that lists the available options. The
    'required' aesthetics cannot be passed on to the `params`. Please
    note that while passing unmapped aesthetics as vectors is
    technically possible, the order and required length is not
    guaranteed to be parallel to the input data.

  - When constructing a layer using a `stat_*()` function, the `...`
    argument can be used to pass on parameters to the `geom` part of the
    layer. An example of this is
    `stat_density(geom = "area", outline.type = "both")`. The geom's
    documentation lists which parameters it can accept.

  - Inversely, when constructing a layer using a `geom_*()` function,
    the `...` argument can be used to pass on parameters to the `stat`
    part of the layer. An example of this is
    `geom_area(stat = "density", adjust = 0.5)`. The stat's
    documentation lists which parameters it can accept.

  - The `key_glyph` argument of
    [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html) may
    also be passed on through `...`. This can be one of the functions
    described as [key
    glyphs](https://ggplot2.tidyverse.org/reference/draw_key.html), to
    change the display of the layer in the legend.

- stat:

  that's "capellini"!

- box.colour:

  aesthetic property of the box

- box.linetype:

  aesthetic property of the box

- box.fill:

  aesthetic property of the box

- box.linewidth:

  aesthetic property of the box

- box.alpha:

  aesthetic property of the box

- line.colour:

  aesthetic property of the reference line

- line.linetype:

  aesthetic property of the reference line

- line.linewidth:

  aesthetic property of the reference line

- line.alpha:

  aesthetic property of the reference line

## Value

A
[`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
object for use in a ggplot.

## `tf` aesthetic

Mandatory. Used to designate a column of class `tf` to be visualized as
glyphs.

## See also

[`GGally::glyphs()`](https://ggobi.github.io/ggally/reference/glyphs.html)

Other tidyfun visualization:
[`autoplot.tf()`](https://tidyfun.github.io/tidyfun/reference/autoplot.tf.md),
[`gglasagna()`](https://tidyfun.github.io/tidyfun/reference/gglasagna.md),
[`ggspaghetti`](https://tidyfun.github.io/tidyfun/reference/ggspaghetti.md)

## Examples

``` r
library(ggplot2)
weather <- fda::CanadianWeather
canada <- data.frame(
  place = weather$place,
  region = weather$region,
  lat = weather$coordinates[, 1],
  lon = -weather$coordinates[, 2],
  region = weather$region
)
canada$temp <- tfd(t(weather$dailyAv[, , 1]), arg = 1:365)
canada$precipl10 <- tfd(t(weather$dailyAv[, , 3]), arg = 1:365) |> tf_smooth()
#> Using `f = 0.15` as smoother span for `lowess()`.
canada_map <-
  data.frame(maps::map("world", "Canada", plot = FALSE)[c("x", "y")])
# map of canada with annual temperature averages in red, precipitation in blue:
ggplot(canada, aes(x = lon, y = lat)) +
  geom_capellini(aes(tf = precipl10), width = 3, height = 5, colour = "blue") +
  geom_capellini(aes(tf = temp), width = 3, height = 5, colour = "red") +
  geom_path(data = canada_map, aes(x = x, y = y), alpha = 0.1) +
  coord_quickmap()


ggplot(canada, aes(x = lon, y = lat, colour = region)) +
  geom_capellini(aes(tf = precipl10),
    width = 5, height = 3,
    line.linetype = 1, box.fill = "white", box.alpha = 0.5, box.colour = NA
  )
```
