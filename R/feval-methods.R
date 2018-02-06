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

print.fvector <- function(x, ...) {
  cat(paste0("fvector[",length(x),"] on (", domain(x)[1], ",", 
    domain(x)[2], ")"))
  invisible(x)
}

print.feval_reg <- function(x, ...) {
  NextMethod()
  cat(" based on", length(argvals(x)), "evaluations each\n")
  cat("interpolation by ", interpolator(x), "\n")
  cat(format(x, ...))
  invisible(x)
}

print.feval_irreg <- function(x, ...) {
  NextMethod()
  n_evals <- n_evaluations(x)
  cat(paste0(" based on ", min(n_evals), " to ", max(n_evals)," (mean: ", 
    round(mean(n_evals)),") evaluations each\n"))
  cat("interpolation by ", interpolator(x), "\n")
  cat(format(x, ...))
  invisible(x)
}

string_rep_feval <- function(argvals, evaluations, use = 5, digits = NULL) {
   digits <- digits %||% options()$digits
   use <- min(use, length(argvals))
   str <- paste(paste0("(", signif(argvals[1:use], digits), ", ",
     signif(evaluations[1:use], digits), ")"), collapse =";")
   if (use < length(argvals)) str <- paste0(str, "; ...")
   paste(str, "\n")
}

format.feval <- function(x, ...){
  argvals <- argvals(x) 
  if (!is_irreg(x)) argvals <- list(argvals) 
  str <- map2_chr(argvals, evaluations(x), string_rep_feval, ... = ...)
  map2_chr(names(x)[1:length(str)], str, ~ paste0(.x,": ",.y))
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
  if (missing(value) | (missing(i) & missing(j))) stop("wtf...?")
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
    # TODO: allow array indices as for arrays, i.e. first dim is i, second j?
    # TODO: allow named-list args where names are used for i and entries are j-vectors?
    stopifnot(inherits(value, class(x)[1]), 
      all(domain(x) == domain(value)),
      identical(interpolator(x), interpolator(value), ignore.environment = TRUE),
      length(value) %in% c(1, length(i)))
    if (inherits(x, "feval_reg")) {
      assert_true(identical(argvals(x), argvals(value)))
    }
    attr_x <- attributes(x)
    attr_x$range <- range(range(x), range(value))
    attr_x$names[i] <- names(value)
    x <- unclass(x)
    x[i] <- unclass(value)
    # if min(i) > length(x) fill up NULL entries with empty functions
    null_entries <- which(sapply(x, is.null))
    if (length(null_entries)) {
      arg_null <- if (inherits(x, "feval_reg")) list(attr_x$argvals) else list(numeric(0))
      x[null_entries] <- 
        unclass(new_feval(arg_null, replicate(length(null_entries), NULL), 
          regular = TRUE, NA, NA, function(v) rep(NA, length(v))))
    }
    attributes(x) <- attr_x
    return(x)
  } 
  # TODO: asIs --> overwrite j-th evaluations with value
  # TODO: if null entries, fill up with empty functions....
  # new i -> create new fevals ( regularity; update domain, range,)
  # old i -> join argvals with j, data with x, warn & overwrite for duplicate j
  # (update regularity & warn; update domain, range,)
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

