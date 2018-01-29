#'@import checkmate

#-------------------------------------------------------------------------------
# new generics & methods 
argvals <- function(f) UseMethod("argvals")
argvals.default <- function(f) .NotYetImplemented()
argvals.feval_irreg <- function(x) map(x, ~ environment(.x)$.argvals)
argvals.feval_reg <- function(x) parent.env(environment(x[[1]]))$.argvals

evaluations <- function(f) UseMethod("evaluations")
evaluations.default <- function(f) .NotYetImplemented()
evaluations.function <- function(f) environment(f)$.data
evaluations.feval <- function(f) map(f, evaluations)

n_evaluations <- function(f) UseMethod("n_evaluations")
n_evaluations.default <- function(f) .NotYetImplemented()
n_evaluations.feval_irreg <- function(f) {
  unlist(map(f, ~ length(environment(.x)$.argvals)))
}
n_evaluations.feval_reg <- function(f) {
  length(parent.env(environment(f[[1]]))$.argvals)
}

domain <- function(f) attr(f, "domain")
interpolator <- function(f) attr(f, "interpolator")

`interpolator<-` <- function(f, value) {
  stopifnot(inherits(f, "feval"), is.function(value))
  f <- map(f, function(.f) environment(.f)$interpolator <- value)
  attr(f, "interpolator") <- attr(value, "label") %||% substitute(value)
}

#-------------------------------------------------------------------------------
# new methods
range.fvector <- function(x, na.rm = FALSE) attr(x, "range")

print.fvector <- function(x) {
  cat(paste0("fvector[",length(x),"] on (", domain(x)[1], ",", 
    domain(x)[2], ")"))
  invisible(x)
}
print.feval_reg <- function(x) {
  NextMethod()
  cat(" based on", length(argvals(x)), "evaluations each\n")
  cat("interpolation by ", interpolator(x), "\n")
  invisible(x)
}
print.feval_irreg <- function(x) {
  NextMethod()
  n_evals <- n_evaluations(x)
  cat(paste0(" based on ", min(n_evals), " to ", max(n_evals)," (mean: ", 
    round(mean(n_evals)),") evaluations each\n"))
  cat("interpolation by ", interpolator(x), "\n")
  invisible(x)
}

#summary #define Arith-methods first.... 
# c.feval_reg #???
`[.feval` <- function(x, i, j, ..., raw = FALSE) {
  if (missing(i)) {
    i <- seq_along(x)
  } else {
    assert_integerish(i, lower = -length(x), upper = length(x), 
      any.missing = FALSE)
    assert_true(all(sign(i) == sign(i)[1]))
  }  
  if (missing(j)) {
    ret <- unclass(x)[i]
    attributes(ret) <- append(attributes(x)[names(attributes(x)) != "names"], 
      list(names = names(ret)))
    return(ret)
    
  }
  assert_numeric(j, any.missing = FALSE, finite = TRUE, min.len = 1)
  outside_domain <- j < domain(x)[1] | j > domain(x)[2]
  if (any(outside_domain)) warning("some <j> outside domain, returning NAs.")
  argvals <- argvals(x)
  if (!is.list(argvals)) {
    argvals <- list(argvals)
  } else argvals <- argvals[i]  
  if (inherits(j, "AsIs")) {
    new_argvals <- map2(list(j), argvals, ~ !(.x %in% .y))
    if (any(unlist(new_argvals))) {
      warning("no data for some of <j>, returning NAs.")
    }
    class(j) <- class(j)[-1]
  } else new_argvals <- list(FALSE)
  return_na <- map2(list(outside_domain), new_argvals, ~ .x | .y)
  ret <- pmap(list(.f = unclass(x)[i], .v = list(j), .na = return_na), 
    function(.f, .v, .na) {
      fv <- .v; fv[.na] <- NA; fv[!.na] <- .f(.v[!.na])
      bind_cols(argvals = .v, data = fv)
  })
  if (raw) {
    ret <- structure(
      do.call(rbind, map(ret, ~ unlist(.x$data))), argvals = j)
  }
  ret
} 

#'@export
`[<-.feval` <- function(x, i, j, value) {
  #if (missing(value)) stop("wtf...?")
  if (missing(i)) {
    i <- seq_along(x)
  } else {
    assert_integerish(i, lower = -length(x), 
      any.missing = FALSE)
    assert_true(all(sign(i) == sign(i)[1]))
    if (sign(i)[1] < 0) {
      i <- (1:length(x))[i]
    }
  }
  if (missing(j)) {
    stopifnot(inherits(value, class(x)[1]), 
      all(domain(x) == domain(value)),
      identical(interpolator(x), interpolator(value)),
      length(value) %in% c(1, length(i)))
    if (inherits(x, "feval_reg")) {
      assert_true(identical(argvals(x), argvals(value)))
    }
    attr_x <- attributes(x)
    attr_x$range <- range(range(x), range(value))
    attr_x$names[i] <- names(value)
    x <- unclass(x)
    x[i] <- unclass(value)
    attributes(x) <- attr_x
    return(x)
  } else .NotYetImplemented()
  
}

# plot
# length
# deriv
# mean
# quantile
# var
# sd
# cov
# cor
# max
# min

### new generics:
# integrate
