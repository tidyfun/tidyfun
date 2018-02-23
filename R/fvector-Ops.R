# *, / for fvectors; and +, -, ^ for fevals
fun_op <- function(x, y, op, numeric = NA){
  if (!is.na(numeric)) {
    num <- list(x, y)[[numeric]]
    f <- list(x, y)[[3 - numeric]]
    assert_numeric(num)
    # no "recycling" of args -- breaking a crappy R convention, proudly so. 
    stopifnot(length(num) %in% c(1, length(f)))
    attr_ret <- attributes(f)
  } else {
    stopifnot(
      # no "recycling" of args
      (length(x) %in% c(1, length(y))) | (length(y) %in% c(1, length(x))),
      all.equal(domain(x), domain(y)),
      all.equal(argvals(x), argvals(y)))
    attr_ret <- attributes(y)
  }
  ret <- map2(x, y, ~ do.call(op, list(e1 = .x, e2 = .y)))
  attributes(ret) <- attr_ret
  if (is_feval(ret)) {
    if (is.na(numeric) && (attr(x, "evaluator_name") != attr(y, "evaluator_name"))) {
      warning("inputs have different evaluators, result has ", 
        attr(ret, "evaluator_name"))
    }
    forget(attr(ret, "evaluator"))
  }  
  return(ret)
}

compare_fvector_attribs <- function(e1, e2, names = FALSE) {
  a1 <- attributes(e1)
  a2 <- attributes(e2)
  attribs <- union(names(a1), names(a2))
  if (!names) attribs <- attribs[attribs != "names"]
  .compare <- function(a, b) {
    if (is.null(a) != is.null(b)) return(FALSE)
    suppressWarnings(
    if (is.function(a)) {
      #FIXME: this is not reliable/useful but prob. impossible to solve
      #generally: would need to know which (functional) objects in the enclosure
      #of these functions are relevant for comparison -- comparing all is too
      #strict but comparing none is rather dangerous. Riight now the function
      #bodies all look the same since they share a common wrapper.... Fingers
      #crossed relevant differences get picked up by differences in the label or
      #basis attributes...
      if (is.memoised(a)) {
        identical(environment(a)[["_f"]], environment(b)[["_f"]], 
          ignore.environment = TRUE)
      } else identical(a, b, ignore.environment = TRUE)
    } else {
      if (is.list(a)) {
        all(unlist(map2(a, ensure_list(b), .compare)))
      } else all(a == b)
    })
  }  
  ret <- map(attribs, ~.compare(a1[[.]], a2[[.]]))
  names(ret) <- attribs
  unlist(ret)
}

Ops.fvector <- function(e1, e2) {
  not_defined <- switch(.Generic, 
    `%%` = , `%/%` = ,
    `&` = , `|` = , `!` = , 
    `<` = , `<=` = , `>=` = , `>` = TRUE, FALSE)
  if (not_defined) 
    stop(sprintf("%s not defined for \"fvector\" objects", .Generic))
  if (nargs() == 1) {
    return(fun_op(0, e1, .Generic, numeric = 1))
  }
}
`==.feval` <- function(e1, e2) {
  # no "recycling" of args
  stopifnot((length(e1) %in% c(1, length(e2))) | 
      (length(e2) %in% c(1, length(e1))))
  # not comparing names, as per convention...
  same <- all(compare_fvector_attribs(e1, e2))
  if (!same) return(rep(FALSE, max(length(e1), length(e2))))
  unlist(map2(e1, e2, ~ all(.x == .y)))
}
#need to copy instead of defining ==.fvector s.t. dispatch in Ops works
`==.fbase` <- eval(`==.feval`)


Ops.feval <- function(e1, e2) {
  ret <- NextMethod()
  if (nargs() != 1) {
    if (is_feval(e1) && is_feval(e2)) {
      if (.Generic == "^") {
        stop("^ not defined for \"feval\" objects")
      } else {
        return(fun_op(e1, e2, .Generic))
      }
    }
    if (is_feval(e1) && is.numeric(e2)) {
      return(fun_op(e1, e2, .Generic, numeric = 2))
    }
    if (is_feval(e2) && is.numeric(e1)) {
      return(fun_op(e1, e2, .Generic, numeric = 1))
    }
    stop(sprintf("binary %s not defined for classes %s and %s", 
      .Generic, class(e1)[1], class(e2)[1]))
  }
  ret
}

Ops.fbase <- function(e1, e2) {
  ret <- NextMethod()
  if (nargs() != 1) {
    both_funs <- is_fbase(e1) & is_fbase(e2)
    if (both_funs) {
      stopifnot(all(compare_fvector_attribs(e1, e2)))
    }
    if (both_funs & .Generic %in% c("+", "-")) {
      # just add/subtract coefs for identical bass
      return(fun_op(e1, e2, .Generic))
    } else {
      # ... else convert to feval, compute, refit basis
      if (both_funs) {
        basis_args <- attr(e1, "basis_args")
        eval <- fun_op(feval(e1), feval(e2), .Generic)
      }
      if (is_fbase(e1) && is.numeric(e2)) {
        basis_args <- attr(e1, "basis_args")
        eval <- fun_op(feval(e1), e2, .Generic, numeric = 2)
      }
      if (is_fbase(e2) && is.numeric(e1)) {
        basis_args <- attr(e2, "basis_args")
        eval <- fun_op(e1, feval(e2), .Generic, numeric = 1)
      }
      #TODO : get rid of this crap by overriding mgcv defaults in constructor
      # and NOT having a "basis" argument that gets used for "bs".
      basis <- basis_args$bs %||% 'cr'
      basis_args <- basis_args[names(basis_args) != "bs"]
      return(do.call("fbase", 
        c(list(eval), basis_args, penalized = FALSE))))
    }
  }   
  ret
}
