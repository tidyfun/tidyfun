# deriv

load_all()

set.seed(12)
grid <- seq(-3, 3, l = 201)
expr <- f <- 3:-3 * feval(grid^3, grid) #rgp(3, nugget = 0, argvals = seq(0, 1, l = 101))

plot(fb <- fbase(f, k = 45, bs = "tp"))
plot(fpc <- fpcbase(f))

layout(1:3)
plot(f, col = 1:3)
plot(fd <- deriv(f), col = 1:3)
plot(fdd <- deriv(f, 2), col = 1:3)
f[3, -3:3]
fd[3, -3:3]/3  # 3 * x^2
fdd[3, -3:3]/6

plot(fb, col = 1:7)
plot(deriv(fb, 1), col = 1:7)
plot(deriv(fb, 2), col = 1:7) #so sketchy.....

fb[3, -3:3]
fbd[3, -3:3]/3  # 3 * x^2
fbdd[3, -3:3]/6 # 6 * x


plot(fpc, col = 1:7)
plot(fpcd <- deriv(fpc, 1), col = 1:7)
plot(fpcdd <- deriv(fpc, 2), col = 1:7)

fpc[3, -3:3]
fpcd[3, -3:3]/3  # 3 * x^2
fpcdd[3, -3:3]/6 # 6 * x

#-------------------------------------------------------------------------------


deriv.feval <- function(expr, order = 1, argvals = NULL, ...) {
  #TODO: should this interpolate back to the original grid?
  data <- as.matrix(expr, argvals = argvals, interpolate = TRUE)
  argvals <- as.numeric(colnames(data))
  derived <- deriv_matrix(data, argvals, order)
  ret <- feval(derived$data, derived$argvals, 
    domain = domain(expr), signif = attr(expr, "signif_argvals"))
  evaluator(ret) <- attr(expr, "evaluator_name")
  ret
}

deriv_matrix <- function(data, argvals, order) {
  for (i in 1:order) {
    delta <- diff(argvals)
    data <- t(diff(t(data))/delta)
    argvals <- (argvals[-1] + head(argvals,-1))/2
  }
  list(data = data, argvals = argvals)
}



deriv.fbase <- function(expr, order = 1, ...) {
  if (grepl("s\\(argvals", attr(expr, "basis_label"))) {
    deriv_fbase_mgcv(expr, order = order, ...)
  }
  if (grepl("FPC", attr(expr, "basis_label"))) {
    deriv_fbase_fpc(expr, order = order, ...)
  }
}

deriv_fbase_mgcv <- function(expr, order = 1, ...) {
  #TODO: make this work for iterated application deriv(deriv(fb)) 
  argvals <- argvals(expr)
  s_args <- attr(expr, "basis_args")
  s_call <- as.call(c(quote(s), quote(argvals), s_args))
  s_spec <- eval(s_call)
  spec_object <- smooth.construct(s_spec, 
    data = data.frame(argvals = argvals), knots = NULL)
  eps <- min(diff(argvals))/1000
  basis_constructor <- smooth_spec_wrapper(spec_object, deriv = order, eps = eps)
  attr(expr, "basis") <- memoise::memoise(basis_constructor)
  attr(expr, "basis_label") <- deparse(s_call, width.cutoff = 60)[1]
  attr(expr, "basis_args") <- s_args
  attr(expr, "basis_matrix") <- basis_constructor(argvals)
  expr
}
deriv_fbase_fpc <- function(expr, order = 1, ...) {
  efunctions <- environment(attr(expr, "basis"))$efunctions
  environment(attr(expr, "basis")) <- new.env()
  dfunctions <- deriv(efunctions, order = order)
  environment(attr(expr, "basis"))$efunctions <- dfunctions
  attr(expr, "basis_matrix") <- t(as.matrix(dfunctions))
  attr(expr, "argvals") <- argvals(dfunctions)
  expr
}

  
