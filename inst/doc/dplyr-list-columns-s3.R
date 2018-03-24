#-------------------------------------------------------------------------------

library(devtools)
library(tidyverse)
load_all()
set.seed(112)
data_irreg <- data.frame(id = rep(1:4, each = 21),
  argvals = as.vector(replicate(4, jitter(seq(0,1, l = 21), amount= .05))),
  data = dbeta(rep(seq(0, 1, l = 21), times = 4), 3, 7))
fi <- feval(data_irreg, evaluator = approx_fill_extend)
test_df <- data_frame(id = 1:4, g = gl(2, 2), 
  fi = fi, f = feval(fi, interpolate = TRUE, argvals = seq(0,1, l = 21)),
  fb = fbase(fi, k = 10))

means <- test_df  %>%  group_by(g) %>%  mutate(mf = mean(f), mfb = median(fb), 
  mfi = mean(fi, na.rm = TRUE))
plot(test_df$fi, col = as.numeric(test_df$g))
lines(means$mfb, col = as.numeric(means$g), lwd = 4)

test_df <-  with(refund::DTI,  
  data_frame(id = ID, sex = sex, pasat = pasat,
    fi_rcst = feval(rcst, argvals = seq(0, 1, l = 55)))) %>% 
  mutate(
    f_rcst = feval(fi_rcst, interpolate = TRUE, argvals = seq(0,1, l = 31)), 
    fb_rcst = fbase(f_rcst, k = 25))
test_df %<>% filter(!is.na(pasat)) %>% group_by(sex) %>%  arrange(pasat) %>% 
  mutate(m = cumsum(f_rcst))

means <- test_df %>% group_by(sex) %>% mutate(mf = mean(f_r), mfb = mean(fb_cca))
plot(test_df$m)
lines(means$mfb, col = as.numeric(means$sex), lwd = 4)



