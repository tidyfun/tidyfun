
``` r
library(rbenchmark)
library(profvis)
library(git2r)
git2r::checkout(branch = "119-no-memo") #@975f15
devtools::load_all()
```

    ## ℹ Loading tidyfun

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

``` r
set.seed(1312)
n <- 100
g <- 1000
arg <- seq(0, 1, l = g)

tfd_lin <- tf_rgp(n = n, arg = arg) |> tfd(resolution = 1e-16)
tfd_sp <- tfd(tfd_lin, evaluator = tf_approx_spline)
tfd_const <- tfd(tfd_lin, evaluator = tf_approx_locf)

tfb_cr10 <- tfb(tfd_lin, k = 10)
```

    ## Percentage of input data variability preserved in basis representation
    ## (per functional observation, approximate):
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   98.40   99.80   99.90   99.79   99.90  100.00

``` r
tfb_ps10 <- tfb(tfd_lin, k = 10, bs = "ps")
```

    ## Percentage of input data variability preserved in basis representation
    ## (per functional observation, approximate):
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   98.40   99.80   99.90   99.79   99.90  100.00

``` r
tfb_tp10 <- tfb(tfd_lin, k = 10, bs = "tp")
```

    ## Percentage of input data variability preserved in basis representation
    ## (per functional observation, approximate):
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   98.40   99.78   99.90   99.78   99.90  100.00

``` r
tfb_cr50 <- tfb(tfd_lin, k = 50)
```

    ## Percentage of input data variability preserved in basis representation
    ## (per functional observation, approximate):
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   98.40   99.80   99.90   99.79   99.90  100.00

``` r
tfb_ps50 <- tfb(tfd_lin, k = 50, bs = "ps")
```

    ## Percentage of input data variability preserved in basis representation
    ## (per functional observation, approximate):
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   98.40   99.80   99.90   99.79   99.90  100.00

``` r
tfb_tp50 <- tfb(tfd_lin, k = 50, bs = "tp")
```

    ## Percentage of input data variability preserved in basis representation
    ## (per functional observation, approximate):
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   98.40   99.80   99.90   99.79   99.90  100.00

``` r
domain <- tf_domain(tfd_lin)
rez <- tf_resolution(tfd_lin)
arg_new <- tfd_lin[1] |> tf_jiggle() |> tf_arg()
```

# benchmark time/memory for tfb and tfd\_reg for repeated identical arg

``` r
bench::mark(
   tf_evaluate(tfd_lin, arg_new) |> length(),
   tf_evaluate(tfd_sp, arg_new) |> length(),
   tf_evaluate(tfd_const, arg_new) |> length(),
)
```

    ## Warning: Some expressions had a GC in every iteration; so filtering is disabled.

    ## # A tibble: 3 × 6
    ##   expression                                   min   median `itr/sec` mem_alloc
    ##   <bch:expr>                              <bch:tm> <bch:tm>     <dbl> <bch:byt>
    ## 1 length(tf_evaluate(tfd_lin, arg_new))      110ms    110ms      5.34    75.6MB
    ## 2 length(tf_evaluate(tfd_sp, arg_new))     110.9ms  115.3ms      8.53    77.5MB
    ## 3 length(tf_evaluate(tfd_const, arg_new))   90.5ms   95.2ms     10.3     73.2MB
    ## # … with 1 more variable: `gc/sec` <dbl>

``` r
bench::mark(
   tf_evaluate(tfb_cr10, arg_new) |> length(),
   tf_evaluate(tfb_ps10, arg_new) |> length(),
   tf_evaluate(tfb_tp10, arg_new) |> length(),
   tf_evaluate(tfb_cr50, arg_new) |> length(),
   tf_evaluate(tfb_ps50, arg_new) |> length(),
   tf_evaluate(tfb_tp50, arg_new) |> length()
)
```

    ## # A tibble: 6 × 6
    ##   expression                                  min   median `itr/sec` mem_alloc
    ##   <bch:expr>                             <bch:tm> <bch:tm>     <dbl> <bch:byt>
    ## 1 length(tf_evaluate(tfb_cr10, arg_new))   4.13ms    4.5ms     215.     15.6MB
    ## 2 length(tf_evaluate(tfb_ps10, arg_new))   4.54ms   4.86ms     198.       16MB
    ## 3 length(tf_evaluate(tfb_tp10, arg_new))  14.94ms  16.31ms      61.5    16.2MB
    ## 4 length(tf_evaluate(tfb_cr50, arg_new))   4.96ms   5.41ms     180.       21MB
    ## 5 length(tf_evaluate(tfb_ps50, arg_new))   5.17ms   5.96ms     164.     19.8MB
    ## 6 length(tf_evaluate(tfb_tp50, arg_new))  20.34ms  23.57ms      40.1    23.6MB
    ## # … with 1 more variable: `gc/sec` <dbl>

# benchmark time/memory for tfb and tfd\_reg for novel arg

``` r
bench::mark(
   lin = tf_evaluate(tfd_lin, tf_jiggle_args(arg_new, .1)) |> length(),
   sp = tf_evaluate(tfd_sp, tf_jiggle_args(arg_new, .1)) |> length(),
   const = tf_evaluate(tfd_const, tf_jiggle_args(arg_new, .1)) |> length(),
)
```

    ## Warning: Some expressions had a GC in every iteration; so filtering is disabled.

    ## # A tibble: 3 × 6
    ##   expression      min   median `itr/sec` mem_alloc `gc/sec`
    ##   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
    ## 1 lin           116ms    125ms      7.88    72.1MB     15.8
    ## 2 sp            128ms    129ms      6.98    74.5MB     17.5
    ## 3 const         104ms    142ms      7.08    70.2MB     14.2

``` r
bench::mark(
   cr10 = tf_evaluate(tfb_cr10, tf_jiggle_args(arg_new, .1)) |> length(),
   ps10 = tf_evaluate(tfb_ps10, tf_jiggle_args(arg_new, .1)) |> length(),
   tp10 = tf_evaluate(tfb_tp10, tf_jiggle_args(arg_new, .1)) |> length(),
   cr50 = tf_evaluate(tfb_cr50, tf_jiggle_args(arg_new, .1)) |> length(),
   ps50 = tf_evaluate(tfb_ps50, tf_jiggle_args(arg_new, .1)) |> length(),
   tp50 = tf_evaluate(tfb_tp50, tf_jiggle_args(arg_new, .1)) |> length()
)
```

    ## # A tibble: 6 × 6
    ##   expression      min   median `itr/sec` mem_alloc `gc/sec`
    ##   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
    ## 1 cr10         4.91ms   5.69ms     159.      4.7MB    30.7 
    ## 2 ps10         4.66ms   6.01ms     152.     4.81MB    13.5 
    ## 3 tp10        16.31ms   20.9ms      41.2    4.95MB     2.06
    ## 4 cr50         5.72ms    7.1ms     125.     5.97MB    14.4 
    ## 5 ps50         5.69ms   6.23ms     155.     5.76MB    23.3 
    ## 6 tp50        21.79ms  25.06ms      39.7    7.12MB     4.42
