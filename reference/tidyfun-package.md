# tidyfun: Tidy Functional Data Wrangling and Visualization

`tidyfun` makes data wrangling and exploratory analysis for functional
data in `R` easier.  
  
`tidyfun` is based on the classes and methods defined in package `tf`
and provides:

- new data types for representing functional data:
  [`tf::tfd()`](https://tidyfun.github.io/tf/reference/tfd.html) &
  [`tf::tfb()`](https://tidyfun.github.io/tf/reference/tfb.html)

- arithmetic & comparison operators
  ([`tf::tfgroupgenerics()`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.html)),

- descriptive statistics: e.g.
  [`tf::mean.tf()`](https://tidyfun.github.io/tf/reference/tfsummaries.html),
  [`tf::median.tf()`](https://tidyfun.github.io/tf/reference/tfsummaries.html)

- and graphics functions for such data:
  [`tf::plot.tf()`](https://tidyfun.github.io/tf/reference/tfviz.html),
  [`autoplot.tf()`](https://tidyfun.github.io/tidyfun/reference/autoplot.tf.md),
  [`geom_spaghetti()`](https://tidyfun.github.io/tidyfun/reference/ggspaghetti.md),
  [`gglasagna()`](https://tidyfun.github.io/tidyfun/reference/gglasagna.md)

- functions to do smoothing
  ([`tf::tf_smooth.tfd()`](https://tidyfun.github.io/tf/reference/tf_smooth.html)),
  differentiation and integration
  ([`tf::tf_derive.tfd()`](https://tidyfun.github.io/tf/reference/tf_derive.html))

- `tidyverse`-verbs for handling functional data inside data frames:
  [`tf_gather()`](https://tidyfun.github.io/tidyfun/reference/tf_gather.md)
  etc.  
    
    
  Also see `vignette("Introducing tidyfun", "tidyfun")` for a brief
  introduction.

## See also

Useful links:

- <https://github.com/tidyfun/tidyfun>

- <https://tidyfun.github.io/tidyfun/>

- Report bugs at <https://github.com/tidyfun/tidyfun/issues>

## Author

**Maintainer**: Fabian Scheipl <fabian.scheipl@googlemail.com>
([ORCID](https://orcid.org/0000-0001-8172-3603)) \[copyright holder\]

Authors:

- Jeff Goldsmith

- Julia Wrobel ([ORCID](https://orcid.org/0000-0001-6783-1421))

Other contributors:

- Maximilian Mücke ([ORCID](https://orcid.org/0009-0000-9432-9795))
  \[contributor\]
