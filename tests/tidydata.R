library(devtools)
library(testthat)
library(tidyverse)

load_all(".")

################################################################################

dti <- refund::DTI

f_cca <- feval(dti$cca, argvals = seq(0, 1, l = 93))
head(f_cca)

test <- with(dti, data_frame(id = ID, sex = sex, cca = f_cca))
test
#FIXME: tibble does not use any standard method to print 
#  (i.e., it never calls print or format on its columns, AFAICT)
# instead it now does lots of weird shit using "pillar" -- 
# see https://github.com/tidyverse/hms/pull/43 and the pillar README,
# seems one now needs to write a pillar_shaft.feval method


# select patient-visits with CCA-FA at location .7 greater than .6
test %>% filter(cca[, .7, matrix = TRUE] > .6)
# select patient-visits with maximal CCA-FA over location interval [.7, 1]  greater than .8
test %>% filter(apply(cca[, seq(.7, 1, l = 50)], 1, max) > .8)

unnest(test, .sep = "_")
unnest(test, .sep = "_", .argvals = seq(0, 1, l = 93)[1:5])
glimpse(test)

test_df <-  with(dti, data.frame(id = ID, sex = sex))
# direct specification of data.frame(id = ID, sex = sex, cca = f_cca) throws errors
# because data.frame calls as.data.frame on columns ....
test_df$cca <- f_cca
head(test_df)
