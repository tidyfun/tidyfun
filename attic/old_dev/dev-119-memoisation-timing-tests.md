
``` r
for (t in list.files("~/fda/tidyfun/tests/testthat", full.names = T)[-1]) {
   git2r::checkout(branch = "119-no-memo") #@975f15
   devtools::load_all()
   t_no <- replicate(10, 
                     system.time(
                        testthat::test_file(t, reporter = FailReporter)
                        )["elapsed"])
   git2r::checkout(branch = "dev") # @ba48336f
   devtools::load_all()
   t_yes <- replicate(10, 
                      system.time(
                         testthat::test_file(t, reporter = FailReporter)
                        )["elapsed"])
   cat(t, ":\n ") 
   print(summary(t_no/t_yes))
}
```

    ## ℹ Loading tidyfun

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

    ## ℹ Loading tidyfun

    ## /home/fabians/fda/tidyfun/tests/testthat/test-calculus.R :
    ##     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.6133  0.8127  0.8870  0.8895  0.9182  1.3786

    ## ℹ Loading tidyfun
    ## ℹ Loading tidyfun

    ## /home/fabians/fda/tidyfun/tests/testthat/test-depth.R :
    ##     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.5657  0.8022  0.9312  1.0249  1.3296  1.5928

    ## ℹ Loading tidyfun
    ## ℹ Loading tidyfun

    ## /home/fabians/fda/tidyfun/tests/testthat/test-evaluate.R :
    ##     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.8678  0.9344  0.9830  0.9797  1.0331  1.0868

    ## ℹ Loading tidyfun
    ## ℹ Loading tidyfun

    ## /home/fabians/fda/tidyfun/tests/testthat/test-evaluator.R :
    ##     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.2709  0.2781  0.2819  0.2881  0.2964  0.3171

    ## ℹ Loading tidyfun
    ## ℹ Loading tidyfun

    ## /home/fabians/fda/tidyfun/tests/testthat/test-names.R :
    ##     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.7464  0.9313  0.9782  0.9821  1.0110  1.3261

    ## ℹ Loading tidyfun
    ## ℹ Loading tidyfun

    ## /home/fabians/fda/tidyfun/tests/testthat/test-ops.R :
    ##     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.7447  0.8047  0.9272  0.9395  1.0452  1.2632

    ## ℹ Loading tidyfun
    ## ℹ Loading tidyfun

    ## /home/fabians/fda/tidyfun/tests/testthat/test-tfb-fpc.R :
    ##     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.8640  0.9566  0.9937  0.9922  1.0166  1.1048

    ## ℹ Loading tidyfun
    ## ℹ Loading tidyfun

    ## /home/fabians/fda/tidyfun/tests/testthat/test-tfb-spline.R :
    ##     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.5532  0.6370  0.6850  0.6888  0.7454  0.8679

    ## ℹ Loading tidyfun
    ## ℹ Loading tidyfun

    ## /home/fabians/fda/tidyfun/tests/testthat/test-tidyr.R :
    ##     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.5333  0.7453  0.8029  0.8239  0.8695  1.1187

    ## ℹ Loading tidyfun
    ## ℹ Loading tidyfun

    ## /home/fabians/fda/tidyfun/tests/testthat/test-vctrs.R :
    ##     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.9972  1.0709  1.1101  1.1483  1.1439  1.4629

    ## ℹ Loading tidyfun
    ## ℹ Loading tidyfun

    ## /home/fabians/fda/tidyfun/tests/testthat/test-where.R :
    ##     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.6612  0.7960  0.9258  0.9279  0.9982  1.3087

    ## ℹ Loading tidyfun
    ## ℹ Loading tidyfun

    ## /home/fabians/fda/tidyfun/tests/testthat/test-zoom.R :
    ##     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.8233  0.9841  1.0955  1.0974  1.1972  1.4180
