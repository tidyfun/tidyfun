# Diffusion tensor imaging data

Fractional anisotropy (FA) tract profiles for the corpus callosum (cca)
and the right corticospinal tract (rcst). Accompanying the tract
profiles are the subject ID numbers, visit number, total number of
scans, multiple sclerosis case status and Paced Auditory Serial Addition
Test (pasat) score.

Data are also included in the
[refund](https://CRAN.R-project.org/package=refund) package in another
format.

## Usage

``` r
dti_df
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 382
rows and 6 columns.

## Details

If you use this data as an example in written work, please include the
following acknowledgment: "The MRI/DTI data were collected at Johns
Hopkins University and the Kennedy-Krieger Institute"

## See also

Other tidyfun datasets:
[`chf_df`](https://tidyfun.github.io/tidyfun/reference/chf_df.md)

## Examples

``` r
dti_df
#> # A tibble: 382 × 6
#>       id visit sex    case                                            cca
#>    <dbl> <int> <fct>  <fct>                                   <tfd_irreg>
#>  1  1001     1 female control (0.000,0.49);(0.011,0.52);(0.022,0.54); ...
#>  2  1002     1 female control (0.000,0.47);(0.011,0.49);(0.022,0.50); ...
#>  3  1003     1 male   control (0.000,0.50);(0.011,0.51);(0.022,0.54); ...
#>  4  1004     1 male   control (0.000,0.40);(0.011,0.42);(0.022,0.44); ...
#>  5  1005     1 male   control (0.000,0.40);(0.011,0.41);(0.022,0.40); ...
#>  6  1006     1 male   control (0.000,0.45);(0.011,0.45);(0.022,0.46); ...
#>  7  1007     1 male   control (0.000,0.55);(0.011,0.56);(0.022,0.56); ...
#>  8  1008     1 male   control (0.000,0.45);(0.011,0.48);(0.022,0.50); ...
#>  9  1009     1 male   control (0.000,0.50);(0.011,0.51);(0.022,0.52); ...
#> 10  1010     1 male   control (0.000,0.46);(0.011,0.47);(0.022,0.48); ...
#> # ℹ 372 more rows
#> # ℹ 1 more variable: rcst <tfd_irreg>
```
