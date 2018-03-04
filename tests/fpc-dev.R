library(devtools)
load_all()


raw <- rgp(25, scale = .01)
f <- fpc_fbase(as.data.frame(raw))
fr <- fpc_fbase(as.data.frame(raw), smooth = FALSE)

fb <- fbase(data, k = 35)
