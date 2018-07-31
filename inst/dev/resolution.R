

domain <- c(0, 1)
resolution <- 0.01

arg <- c(0, 0.11, 0.111, 0.2, 0.351, 1)
evaluations <- value <- arg
f <- tfd(arg, evaluations, resolution = .001)

object <- tfd(arg, evaluations)
f <- zoo::na.approx

x <- c(0, .2, .211, .2111, .21111, .21112)
x <- adjust_resolution(x, object, unique = FALSE)

x_arg <- sort(unique(c(x, arg)))
x_arg_match <- match(x_arg, arg, nomatch = length(arg) + 1)
requested <-  x_arg %in% x
ret <- f(zoo::zoo(evaluations[x_arg_match], x_arg))
zoo::coredata(ret)[requested]
