#'@import checkmate
argvals <- function(x) UseMethod("argvals")
argvals.feval_irreg <- function(x) map(x, ~ environment(.x)$.argvals)
argvals.feval_reg <- function(x) parent.env(environment(x[[1]]))$.argvals

evaluations <- function(f) UseMethod("evaluations")
evaluations.function <- function(f) environment(f)$.data
evaluations.feval <- function(f) map(f, evaluations)

domain <- function(x) attr(x, "domain")
range.fvector <- function(x) attr(x, "range")

print.fvector <- function(x) {
  cat(paste0("fvector[",length(x),"] on (", domain(x)[1], ",", 
    domain(x)[2], ")"))
  invisible(x)
}
print.feval_reg <- function(x) {
  NextMethod()
  cat(" based on", length(argvals(x)), "evaluations each.\n")
  invisible(x)
}
print.feval_irreg <- function(x) {
  NextMethod()
  n_evals <- sapply(argvals(x), length)
  cat(paste0(" based on ", min(n_evals), " to ", max(n_evals)," (mean: ", 
    round(mean(n_evals)),") evaluations each.\n"))
  invisible(x)
}

#summary #define Arith-methods first.... 
# c.feval_reg #???
`[.feval` <- function(x, i, j, ..., raw = FALSE, interpolate = TRUE) {
  if (missing(i)) {
    i <- seq_along(x)
  } else {
    assert_integerish(i, lower = -length(x), upper = length(x), 
      any.missing = FALSE)
    assert_true(all(sign(i) == sign(i)[1]))
  }  
  if (missing(j)) {
    ret <- unclass(x)[i]
    attributes(ret) <- c(names = names(ret), 
      attributes(x)[names(attributes(x)) != "names"])
    return(ret)
  }
  assert_numeric(j, any.missing = FALSE, finite = TRUE, min.len = 1)
  outside_domain <- j < domain(x)[1] | j > domain(x)[2]
  if (any(outside_domain)) warning("some <j> outside domain, returning NAs.")
  argvals <- argvals(x)
  if (!is.list(argvals)) {
    argvals <- list(argvals)
  } else argvals <- argvals[i]  
  if (!interpolate) {
    new_argvals <- map2(list(j), argvals, ~ !(.x %in% .y))
    if (any(unlist(new_argvals))) {
      warning("some <j> not part of argvals, returning NAs.")
    }  
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
#`[<-`  
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
