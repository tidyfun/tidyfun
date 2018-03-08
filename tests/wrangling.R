#load <- require(tidyfun)
if (!load) {
  library(devtools)
  load_all(".")
  library(testthat)
  library(checkmate)
  library(dplyr)
  library(purrr)
}  

################################################################################

dti <- refund::DTI

f_cca <- feval(dti$cca, argvals = seq(0, 1, l = 93), signif = 2)
f_rcst <- feval(dti$rcst, argvals = seq(0, 1, l = 55))
f_cca
f_rcst

fb_cca <- fbase(dti$cca, k = 25, argvals = seq(0, 1, l = 93))

fb_cca 

plot(fb_cca[1:5])
lines(f_cca[1:5], col = 2, lty = 3)

test_tbl <- with(dti, data_frame(id = ID, sex = sex,  cca = f_cca, rcst = f_rcst))
test_tbl
#FIXME: tibble does not use any standard method to print 
#  (i.e., it never calls print or format on its columns, AFAICT)
# instead it now does lots of weird shit using "pillar" -- 
# see https://github.com/tidyverse/hms/pull/43 and the pillar README,
# seems one now needs to write a pillar_shaft.feval method


# select patient-visits with CCA-FA at location .7 greater than .6
test_tbl %>% filter(cca[, .7, matrix = TRUE] > .6)
# select patient-visits with maximal CCA-FA over location interval [.7, 1]  greater than .8
test_tbl %>% filter(apply(rcst[, seq(.7, 1, l = 50)], 1, max) > .8)

evaluate(test_tbl)
evaluate(test_tbl, argvals = seq(0,1, l = 12))
evaluate(test_tbl, argvals = seq(0,1, l = 12), cca)

tidyr::unnest(evaluate(test_tbl, argvals = seq(0,1, l = 12)), .preserve = rcst, 
  .sep = "_")
# glimpse(test_tbl) #?!?

#-------------------------------------------------------------------------------

test_df <-  with(dti, data.frame(id = ID, sex = sex))
# direct specification of data.frame(id = ID, sex = sex, cca = f_cca) throws errors
# because data.frame calls as.data.frame on columns ....
test_df$cca <- f_cca
head(test_df)
str(subset(test_df, cca[, .7, matrix = TRUE] > .6))
str(evaluate(test_df[1:4, ]))

# FIXME:
test_df %>% group_by(sex) %>% summarize(mean_cca = mean(cca), sd_cca = sd(cca))
# FIXME:
test <- test_df$cca[1:10]
test[1:2] <- test[c(3,4)]
mean(test)

#-------------------------------------------------------------------------------

library(data.table)
test_dt <- data.table(test_df)
#! does not work because this does not use the correct subsetting-method:
str(test_dt[drop(cca[, .7, matrix = TRUE] > .6), ]) 

# TODO: write more tests: merging, summarizing, mutating etc
