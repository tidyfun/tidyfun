Ops.fvector <- function(e1, e2) {
  not_defined <- switch(.Generic, 
    `%%` = , `%/%` = ,
    `&` = , `|` = , `!` = , 
    `<` = , `<=` = , `>=` = , `>` = TRUE, FALSE)
  if (not_defined) 
    stop(gettextf("%s not defined for \"fvector\" objects", 
      .Generic), domain = NA)
  # unary ops work the same for feval and fbase, return from here
  if (nargs() == 1) {
    return(scalar_fun_op(e1, 0, .Generic))
  } else {
    both_funs <- is_fvector(e1) & is_fvector(e2)
    if (both_funs) {
      stopifnot( (is_feval(e1) & is_feval(e2)) | (is_fbase(e1) & is_fbase(e2)),
        all(domain(e1) == domain(e2)))
    } else {
      # can only add/multiply/subtract etc... scalars or length(n) vectors
      num <- ifelse(is_fvector(e1), e2, e1) 
      assert_numeric(num)
    }
  }
}

scalar_fun_op <- function(f, num, op){
  attr_f <- attributes(f)
  f <- map2(f, num, ~ do.call(op, list(e1 = .y, e2 = .x)))
  attributes(f) <- attr_f
  if (is_feval(f)) forget(attr(f, "evaluator"))
  return(f)
}

Ops.feval <- function(e1, e2) {
  ret <- NextMethod()
  if (nargs() != 1) {
    both_funs <- is_fvector(e1) & is_fvector(e2)
  }
  ret
}


Ops.fbase <- function(e1, e2) {
  ret <- NextMethod()
  if (nargs() != 1) {
    both_funs <- is_fvector(e1) & is_fvector(e2)
  }
  ret
}
