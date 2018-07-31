# **`tidyfun`**

The goal of **`tidyfun`** is to provide accessible and well-documented software 
that makes functional data analysis in `R` easy, specifically:  
data wrangling and exploratory analysis.

**`tidyfun`** provides:  

- new **data types** for representing functional data: **`tfd`** & **`tfb`**
- arithmetic **operators**, descriptive **statistics** and **graphics** functions for such data
- `tidyverse`-verbs for handling functional data **inside** data frames.

## Installation

``` r
devtools::install_github("fabian-s/tidyfun")
```

## What does it do?

#### New vector-like data types for functional data

**`tidyfun`** provides new `S3`-classes for functional data, either as raw data (class `tfd` for *t*idy *f*unctional *d*ata) or in basis representation (class `tfb` for *t*idy *f*unctional *b*asis data). 
Such `tf`s can be subsetted or subassigned, computed on and summarized: almost all

- operators like `==`, `+` or `*`
- math functions like `sum`, `log` or `abs` 
- and statistics functions like `mean` or `sd`  

are defined for **`tidyfun`**'s data structures.

The `tf` objects are basically glorified lists, so they work well as columns in data frames. That makes it a lot easier to keep conventional data and functional measurements together in one object for preprocessing, exploratory analysis and description.

At the same time, they actually behave like vectors of *functions* to some extent, i.e., they can be evaluated on any point of their domain, integrated or differentiated, etc.

#### `tidyverse` verbs for dealing with functional data inside data frames

All `dplyr` verbs work on `tf`-columns, so you can `filter`, `mutate`, `summarize` etc
functional data pretty much like conventional data.
**`tidyfun`** also provides `tf_gather` & `tf_spread`, `tf_nest` & `tf_unnest`
in order to quickly and easily create, reshape and transform functional data columns.

#### New `ggplot2` `geoms` and `stats` for functional data

**`tidyfun`** defines **pasta-themed** `geom`s with a **`tf`**-aesthetic for functional data:

- **`geom_spaghetti`** for lines
- **`geom_meatballs`**  for (lines &) points
- **`gglasagna`** for [lasagna plots](https://asset.jmir.pub/assets/76aeec48564abf0e6f6da8e9cd06346d.png), with an **`order`**-aesthetic to sort the lasagna layers

To come:

- **`geom_pappardelle`** for functional boxplots
- **`geom_capellini`** for little sparklines / [glyphs](http://ggobi.github.io/ggally/#ggallyglyphs) on maps etc. 

----------------------

**See the vignette for code examples.**
