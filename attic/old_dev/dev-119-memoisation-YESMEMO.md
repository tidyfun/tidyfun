
``` r
library(rbenchmark)
library(profvis)
library(git2r)
git2r::checkout(branch = "dev") # @ba48336f
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

    ## # A tibble: 3 × 6
    ##   expression                                   min   median `itr/sec` mem_alloc
    ##   <bch:expr>                              <bch:tm> <bch:tm>     <dbl> <bch:byt>
    ## 1 length(tf_evaluate(tfd_lin, arg_new))     27.1ms   28.7ms      27.4    79.2MB
    ## 2 length(tf_evaluate(tfd_sp, arg_new))      27.1ms   29.6ms      31.4    80.9MB
    ## 3 length(tf_evaluate(tfd_const, arg_new))   28.8ms   32.5ms      31.3    76.6MB
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
    ## 1 length(tf_evaluate(tfb_cr10, arg_new))   4.15ms   4.71ms      199.    16.2MB
    ## 2 length(tf_evaluate(tfb_ps10, arg_new))   3.83ms   4.75ms      154.    16.6MB
    ## 3 length(tf_evaluate(tfb_tp10, arg_new))    4.3ms   5.37ms      143.    16.8MB
    ## 4 length(tf_evaluate(tfb_cr50, arg_new))   4.32ms   5.91ms      134.    21.7MB
    ## 5 length(tf_evaluate(tfb_ps50, arg_new))   4.44ms   4.72ms      160.    20.4MB
    ## 6 length(tf_evaluate(tfb_tp50, arg_new))   3.28ms   5.12ms      190.    24.3MB
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
    ## 1 lin           116ms    121ms      8.27      75MB     13.2
    ## 2 sp            122ms    125ms      7.95    77.4MB     13.9
    ## 3 const         108ms    112ms      8.68    73.1MB     15.6

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
    ## 1 cr10         4.93ms   5.46ms     170.     4.72MB    32.9 
    ## 2 ps10         5.07ms   5.78ms     162.     4.84MB    23.2 
    ## 3 tp10        16.05ms  17.45ms      56.7    4.98MB     4.36
    ## 4 cr50         5.65ms   6.01ms     161.     5.99MB    26.0 
    ## 5 ps50         5.86ms   6.43ms     142.     5.79MB    26.9 
    ## 6 tp50        21.04ms  24.29ms      38.4    7.15MB     4.52
