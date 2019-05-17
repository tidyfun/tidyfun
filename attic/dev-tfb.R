set.seed(17711L)
smoo <- tf_rgp(10, nugget = 0)
rough <- tf_rgp(10, arg = 121L, nugget = .2, scale = .005)
narrow <- tf_jiggle(tf_rgp(10, arg = 11L, nugget = 0))
irr <- tf_sparsify(smoo)


smooexp <- 
  tfb_spline(exp(smoo), family = gaussian(link = "log"), penalized = FALSE)

map(coef(smooexp), ~ attr(smooexp, "basis_matrix") %*% .x)[[1]][1:10]
exp(smoo)[1]

rufsmo <- c(rough, smoo)
debugonce(fit_penalized_ls)

hetero <- c(
  tf_rgp(10, scale = 0.2, nugget = .05, arg = 101L),
  tf_rgp(10, scale = 0.02, nugget = .05, arg = 101L),
  tf_rgp(10, scale = 0.002, nugget = .05, arg = 101L)) %>%
  tf_sparsify(dropout = .1) %>% tf_jiggle()
plot(hetero, points = FALSE, col = rep(2:4, e = 10))
plot(tfb_spline(hetero, k = 11, bs= "tp"), col = 1)
lines(tfb_spline(hetero, k = 41, bs= "ds"), col = 2, lty = 3)
plot(tfb_spline(hetero, k = 21, bs= "gp"), col = 1)

plot(hetero, points = FALSE, col = rep(2:4, e = 10))
lines(tfb_spline(hetero, k = 41, bs= "ps"), col = 1)

layout(t(1:4))
plot(hetero, col = rep(2:4, e = 10), points = FALSE)
plot(tfb_spline(hetero, k = 21, bs= "ds"), col = rep(2:4, e = 10))
plot(tfb_spline(hetero, k = 41, bs= "tp"), col = rep(2:4, e = 20))
set.seed(1212)
plot(tfb_spline(hetero, k = 41, global = TRUE, bs= "ps"), col = rep(2:4, e = 10))
plot(tfb_spline(hetero, k = 41, bs= "ps", sp = 10), col = rep(2:4, e = 10))
plot(tfb_spline(hetero, k = 41, bs= "ps", sp = 1000), col = rep(2:4, e = 10))

hetex <- exp(hetero)
plot(hetex, col = rep(2:4, e = 10), points = FALSE)
plot(tfb_spline(hetex, k = 21, bs= "ps", family = gaussian(link = "log")), 
     col = rep(2:4, e = 10))
plot(tfb_spline(hetex, k = 21, bs= "ps", family = Gamma(link = "log")), 
     col = rep(2:4, e = 10))
plot(tfb_spline(hetex, k = 21, bs= "ps", family = Gamma(link = "inverse")), 
     col = rep(2:4, e = 10), log = "y")

plot(tfb_spline(hetex, k = 41, bs= "ps", family = Gamma(link = "log")), 
     col = rep(2:4, e = 10), log = "y")
plot(tfb_spline(hetex, k = 41, bs= "ps", family = Gamma(link = "log"),
                sp = 1e4), 
     col = rep(2:4, e = 10), log = "y")
plot(tfb_spline(hetex, k = 41, bs= "ps", family = Gamma(link = "log"),
                global = TRUE), 
     col = rep(2:4, e = 10), log = "y")
