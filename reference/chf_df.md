# Congestive heart failure accelerometry data

Activity data from a study of congestive heart failure. Data were
originally presented in "Multilevel Matrix-Variate Analysis and its
Application to Accelerometry-Measured Physical Activity in Clinical
Populations" by Huang et al.; these data are public, with download
information in the paper.

## Usage

``` r
chf_df
```

## Format

An object of class `spec_tbl_df` (inherits from `tbl_df`, `tbl`,
`data.frame`) with 329 rows and 8 columns.

## See also

Other tidyfun datasets:
[`dti_df`](https://tidyfun.github.io/tidyfun/reference/dti_df.md)

## Examples

``` r
chf_df
#> # A tibble: 329 × 8
#>       id gender   age   bmi event_week event_type day  
#>    <dbl> <chr>  <dbl> <dbl>      <dbl> <chr>      <ord>
#>  1     1 Male      41    26         41 .          Mon  
#>  2     1 Male      41    26         41 .          Tue  
#>  3     1 Male      41    26         41 .          Wed  
#>  4     1 Male      41    26         41 .          Thu  
#>  5     1 Male      41    26         41 .          Fri  
#>  6     1 Male      41    26         41 .          Sat  
#>  7     1 Male      41    26         41 .          Sun  
#>  8     3 Female    81    21         32 .          Mon  
#>  9     3 Female    81    21         32 .          Tue  
#> 10     3 Female    81    21         32 .          Wed  
#> # ℹ 319 more rows
#> # ℹ 1 more variable: activity <tfd_reg>
```
