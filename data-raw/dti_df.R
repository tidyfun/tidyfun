###########################################################
# The DTI dataset is a perennial example in refund, and is
# used often in tidyfun examples as well. We therefore 
# include this dataset as an example; this code produces a
# tidy version of the DTI data. The code also appears in a
# vignette showing data conversions.
###########################################################

library(tidyfun)
library(tibble)

dti_df = tibble(
  id = refund::DTI$ID, 
  visit = refund::DTI$visit,
  sex = refund::DTI$sex, 
  case = factor(ifelse(refund::DTI$case, "MS", "control")))

dti_df$cca = tfd(refund::DTI$cca, arg = seq(0,1, l = 93))
dti_df$rcst = tfd(refund::DTI$rcst, arg = seq(0, 1, l = 55))

usethis::use_data(dti_df, overwrite = TRUE)
