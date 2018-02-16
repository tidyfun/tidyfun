#'@import checkmate

#-------------------------------------------------------------------------------
# new generics & methods 
argvals <- function(f) UseMethod("argvals")
argvals.default <- function(f) .NotYetImplemented()
argvals.feval_irreg <- function(f) attr(f, "argvals")
argvals.feval_reg <- function(f) attr(f, "argvals")[[1]]

evaluations <- function(f) UseMethod("evaluations")
evaluations.default <- function(f) .NotYetImplemented()
evaluations.feval <- function(f) {
  attributes(f) <- NULL
  f
}  

n_evaluations <- function(f) UseMethod("n_evaluations")
n_evaluations.default <- function(f) .NotYetImplemented()
n_evaluations.feval_irreg <- function(f) map_int(evaluations(f), length)
n_evaluations.feval_reg <- function(f) length(argvals(f))

domain <- function(f) attr(f, "domain")
evaluator <- function(f) attr(f, "evaluator")

`evaluator<-` <- function(x, value) {
  stopifnot(inherits(x, "feval"), is.function(value))
  attr(x, "evaluator_name") <- deparse(value, width.cutoff = 60)[1]
  attr(x, "evaluator") <- memoise(eval(value))
  x
}

#-------------------------------------------------------------------------------
# new methods
range.fvector <- function(x, na.rm = FALSE) attr(x, "range")

print.fvector <- function(x, n  = 10, ...) {
  cat(paste0("fvector[",length(x),"] on (", domain(x)[1], ",",
    domain(x)[2], ")"))
  invisible(x)
}

print.feval_reg <- function(x, n = 10, ...) {
  NextMethod()
  cat(" based on", length(argvals(x)), "evaluations each\n")
  cat("interpolation by", attr(x, "evaluator_name"), "\n")
  cat(format(x[1:min(n, length(x))], ...), sep = "\n")
  if (n < length(x)) 
    cat(paste0("    [....]   (", length(x) - n, " not shown)\n"))
  invisible(x)
}

print.feval_irreg <- function(x, n = 10, ...) {
  NextMethod()
  n_evals <- n_evaluations(x[!is.na(names(x))])
  cat(paste0(" based on ", min(n_evals), " to ", max(n_evals)," (mean: ",
    round(mean(n_evals)),") evaluations each\n"))
  cat("inter-/extrapolation by", attr(x, "evaluator_name"), "\n")
  cat(format(x[1:min(n, length(x))], ...), sep = "\n")
  if (n < length(x)) 
    cat(paste0("    [....]   (", length(x) - n, " not shown)\n"))
  invisible(x)
}

string_rep_feval <- function(argvals, evaluations, signif_argvals = NULL, show = 5, digits = NULL, ...) {
   digits_eval <- digits %||% options()$digits
   digits_argvals <- max(digits_eval, signif_argvals %||% digits_eval) 
   show <- min(show, length(argvals))
   str <- paste(
     paste0("(", format(argvals[1:show], digits = digits_argvals, trim = TRUE, ...),
       ",",
     format(evaluations[1:show], digits = digits_eval, trim = TRUE, ...), ")"), 
     collapse = ";")
   if (show < length(argvals)) str <- paste0(str, "; ...")
   str
}

# FIXME: this needs proper width align etc arguments like format.default
format.feval <- function(x, digits = 2, nsmall = 0, ...){
   argvals <- attr(x, "argvals") 
   str <- map2_chr(argvals, evaluations(x), string_rep_feval, 
     signif_argvals = attr(x, "signif_argvals"), 
     digits = digits, nsmall = nsmall, ... = ...)
   map2_chr(names(x)[1:length(str)], str, ~ paste0(.x,": ",.y))
}


#summary #define Arith-methods first.... 
# c.feval_reg #???

`[.feval` <- function(x, i, j, interpolate = TRUE, matrix = TRUE) {
  if (missing(i)) {
    i <- seq_along(x)
  } else {
    assert_atomic(i)
    if (is.logical(i)) {
      assert_logical(i, any.missing = FALSE, len = length(x))
      i <- which(i)
    }
    if (is.character(i)) {
      assert_subset(i, names(x))
      i <- match(i, names(x))
    }
    assert_integerish(i, lower = -length(x), upper = length(x), 
      any.missing = FALSE)
    assert_true(all(sign(i) == sign(i)[1]))
  }  
  if (missing(j)) {
    ret <- unclass(x)[i]
    if (is_irreg(x)) attr(x, "argvals") <-  attr(x, "argvals")[i]
    attributes(ret) <- append(attributes(x)[names(attributes(x)) != "names"], 
      list(names = names(ret)))
    return(ret)
  } 
  if (matrix & is.list(j)) {
    stop("need a single vector-valued <j> if matrix = TRUE")
  }
  j <- adjust_resolution(ensure_list(j), x)
  evals <- evaluate(x[i], argvals = j)
  if (!interpolate) {
    new_j <- map2(j, ensure_list(argvals(x)), ~ !(.x %in% .y))
    if (any(unlist(new_j))) {
      warning("interpolate = FALSE & no evaluations for some <j>: NAs created.")
    }
    evals <- map2(evals, new_j, ~ ifelse(.y, NA, .x))
  }
  if (matrix) {
    ret <- do.call(rbind, evals)
    colnames(ret) <- unlist(j)
    rownames(ret) <- names(x)[i]
    structure(ret, argvals = unlist(j))
  } else {
    ret <- map2(j, evals, ~ bind_cols(argvals = .x, data = .y))
    names(ret) <- names(x)[i]
    ret
  }
} 

#'@export
`[<-.feval` <- function(x, i, j, value) {
  if (missing(value) | (missing(i) & missing(j))) stop("wtf...?")
  if (missing(i)) {
    i <- seq_along(x)
  } else {
    assert_atomic(i)
    if (is.logical(i)) {
      assert_logical(i, any.missing = FALSE, len = length(x))
      i <- which(i)
    }
    if (is.character(i)) {
      assert_subset(i, names(x))
      i <- match(i, names(x))
    }
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
      identical(evaluator(x), evaluator(value), ignore.environment = TRUE),
      length(value) %in% c(1, length(i)))
    if (inherits(x, "feval_reg")) {
      assert_true(identical(argvals(x), argvals(value)))
    }
    attr_x <- attributes(x)
    attr_x$range <- range(range(x), range(value))
    attr_x$names[i] <- names(value)
    ret <- unclass(x)
    ret[i] <- unclass(value)
    if (is_irreg(x)) {
      attr_x$argvals[i] <- argvals(value)
    }
    # fill up empty functions
    na_entries <- which(sapply(ret, is.null))
    if (length(na_entries)) {
        na_length <- ifelse(is_irreg(x), 1, length(attr_x$argvals[[1]]))
        ret[na_entries] <- replicate(length(na_entries), rep(1*NA, na_length), 
          simplify = FALSE)
        if (is_irreg(x)) attr_x$argvals[na_entries] <- 
          replicate(length(na_entries), domain(x)[1], simplify = FALSE)
    }
    attributes(ret) <- attr_x
    ret
  } else stop("<j>-argument for '[<-.feval' not implemented yet.")
  # TODO: 
  # new i -> create new fevals ( regularity; update domain, range,)
  # old i -> join argvals with j, data with x, warn & overwrite for duplicate j
  # (update regularity & warn; update domain, range,)
}

# plot
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

