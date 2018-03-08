library(devtools)
load_all()


raw <- rgp(250, scale = .01)
f <- fpc_fbase(as.data.frame(raw))
fr <- fpc_fbase(as.data.frame(raw), smooth = FALSE)
fb <- fbase(raw, k = 35)

object.size(raw)
object.size(f)
object.size(fr)
object.size(fb)

