#'@import checkmate
argvals <- function(x) UseMethod("argvals")
argvals.feval_irreg <- function(x) map(x, ~ environment(.x)$argvals)
argvals.feval_reg <- function(x) parent.env(environment(x[[1]]))$argvals

evaluations <- function(f) UseMethod("evaluations")
evaluations.function <- function(f) environment(f)$data
evaluations.feval <- function(f) map(f, evaluations)

domain <- function(x) attr(x, "domain")
range.fvector <- function(x) attr(x, "range")

print.feval <- function(x) {
  cat(paste0("feval[",length(x),"] on (", domain(x)[1], ",", domain(x)[2], ")\n"))
  cat("evaluated for:\n")
  print(argvals(x))
  invisible(x)
}


#summary #define Arith-methods first.... 
# c.feval_reg #???
`[.feval` <- function(x, i, j, interpolate = TRUE) {
  if (missing(i)) {
    i <- seq_along(x)
  } else {
    assert_integerish(i, lower = -length(x), upper = length(x), any.missing = FALSE)
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
  if(any(outside_domain)) warning("some <j> outside domain, returning NAs.")
  argvals <- argvals(x)
  if(!is.list(argvals)) {
    argvals <- list(argvals)
  } else argvals <- argvals[i]  
  if (!interpolate) {
    new_argvals <- map2(list(j), argvals, ~ !(.x %in% .y))
    if (any(unlist(new_argvals))) {
      warning("some <j> not part of argvals, returning NAs.")
    }  
  } else new_argvals <- list(FALSE)
  return_na <- map2(list(outside_domain), new_argvals, ~ .x | .y)
  pmap(list(.f = unclass(x)[i], .v = list(j), .na = return_na), 
    function(.f, .v, .na) {
      fv <- .v; fv[.na] <- NA; fv[!.na] <- .f(.v[!.na])
      bind_cols(argvals = .v, data = fv)
  })
} 
# `[[.feval` <- function(x, i, j, interpolate = TRUE) {
#   browser()
#   ret <- x[i = i, j = j, interpolate = interpolate]
#   structure(rbind(map(ret, ~.x$data)), argvals = j) 
# }

#`[.feval_irreg`
#`[<-`  
#plot
#length
#deriv
#mean
#quantile
#var
#sd
#cov
#cor
#max
#min

### new generics:
# integrate
