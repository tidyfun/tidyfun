ensure_list <- function(x) if (!is.list(x)) list(x) else x

unique_id <- function(x) if (is.character(x)) make.unique(x) else x

#'@import zoo
zoo_wrapper <- function(f, ...){
  dots <- list(...)
  function(x, argvals, evaluations) {
      x_arg <- sort(unique(c(x, argvals)))
      x_arg_match <- match(x_arg, argvals, nomatch = length(argvals) + 1)
      requested <-  x_arg %in% x
      dots[[length(dots) + 1]] <- zoo(evaluations[x_arg_match], x_arg)
      ret <- do.call(f, dots)
      coredata(ret)[requested]
  }
}
approx_linear <- zoo_wrapper(na.approx, na.rm = FALSE)
approx_spline <- zoo_wrapper(na.spline, na.rm = FALSE)
approx_fill_extend <- zoo_wrapper(na.fill, fill = "extend")
approx_locf <- zoo_wrapper(na.locf, na.rm = FALSE)
approx_nocb <- zoo_wrapper(na.locf, na.rm = FALSE, fromLast = TRUE)

in_range <- function(x, r){
  r <- range(r, na.rm = TRUE)
  x >= r[1] & x <= r[2]
}
na_to_0 <- function(x) {
  x[is.na(x)] <- 0
  x
}

find_argvals <- function(data, argvals) {
  if (is.null(argvals)) {
    suppressWarnings(argvals <- as.numeric(dimnames(data)[[2]]))
    if (is.null(argvals) | any(is.na(argvals))) {
      message("Column names of <data> not suitable as argvals.")
      argvals <- numeric(0)
    }
  } 
  if (!length(argvals)) argvals <- seq_len(dim(data)[2])
  stopifnot(length(argvals)  == dim(data)[2], 
    is.numeric(argvals), all(!is.na(argvals)))
  list(argvals)
}

#' @import checkmate
assert_argvals <- function(argvals, x){
  if (is.list(argvals)) {
    assert_true(length(argvals) %in% c(1, length(x)))
    map(argvals, ~ assert_argvals_vector(., x = x))
  } else {
    assert_argvals_vector(argvals, x)
  }
}
assert_argvals_vector <- function(argvals, x) {
  assert_numeric(argvals, any.missing = FALSE, unique = TRUE,
    lower = domain(x)[1], upper = domain(x)[2])
}



# #TODO: write proper tests for this
# check_interpolation <- function(x, argvals){
#   UseMethod("check_interpolation")
# }
# check_interpolation.feval_reg <- function(x, argvals){
#   original <- argvals(x)
#   if (is.list(argvals)) {
#     map(argvals, ~ !(. %in% original))
#   } else {
#     !(argvals %in% original)
#   }
# }
# check_interpolation.feval_irreg <- function(x, argvals) {
#   original <- argvals(x)
#   if (is.list(argvals)) {
#     map2(argvals, original, ~ !(.x %in% .y))
#   } else {
#     map(original, ~ !(argvals %in% .x))
#   }
# }

adjust_resolution <- function(argvals, f) {
  signif <- attr(f, "signif_argvals")
  .adjust_resolution(argvals, signif)
}

.adjust_resolution <- function(argvals, signif, unique = TRUE){
  u <- if (unique) base::unique else function(x) x
  if (is.list(argvals)) {
    map(argvals, ~ u(signif(., signif)))
  } else {
    u(signif(argvals, signif))
  }  
}

string_rep_fvector <- function(argvals, evaluations, signif_argvals = NULL, show = 5, digits = NULL, ...) {
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

compare_fvector_attribs <- function(e1, e2, ignore = "names") {
# TODO: better way to check evaluator/basis functions?
  a1 <- attributes(e1)
  a2 <- attributes(e2)
  attribs <- union(names(a1), names(a2))
  if (length(ignore)) attribs <- attribs[!(attribs %in% ignore)]
  .compare <- function(a, b) {
    if (is.null(a) != is.null(b)) return(FALSE)
    suppressWarnings(
      if (is.function(a)) {
        #FIXME: this is not reliable/useful but prob. impossible to solve
        #generally: would need to know which (functional) objects in the enclosure
        #of these functions are relevant for comparison -- comparing all is too
        #strict but comparing none is rather dangerous. Right now the function
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
        } else isTRUE(all.equal(a, b))
      })
  }  
  ret <- map(attribs, ~.compare(a1[[.]], a2[[.]]))
  names(ret) <- attribs
  unlist(ret)
}


#' @export
is_fvector <- function(x) "fvector" %in% class(x)
#' @export
is_irreg <- function(x) "feval_irreg" %in% class(x)
#' @export
is_feval <- function(x) "feval" %in% class(x)
#' @export
is_fbase <- function(x) "fbase" %in% class(x)


# used for Summary grup generics and stats-methods...
# op has to be a string!
summarize_fvector <- function(..., op = NULL, eval = FALSE) {
  dots <- list(...)
  funs <- map_lgl(dots, is_fvector)
  op_args <- dots[!funs]
  funs <- dots[funs]
  op_call <- function(x) do.call(op, c(list(x), op_args))
  funs <- do.call(c, funs)
  attr_ret <- attributes(funs)
  m <- as.matrix(funs)
  ret <- apply(m, 2, op_call)
  argvals <- as.numeric(colnames(m))
  args <- c(list(ret), argvals = list(argvals),
    domain = list(domain(funs)), 
    signif = attr(funs, "signif_argvals"))
  if (eval) {
    return(do.call(feval, c(args, evaluator = as.name(attr(funs, "evaluator_name")))))
  } else {
    return(do.call(fbase, c(args, penalized = FALSE, attr(funs, "basis_args"))))
  }
}

#' Random generator for Gaussian Processes
#' 
#' The function generates `n` realizations of Gaussian processes , either with
#' squared exponential covariance \eqn{Cov(x(t), x(t')) = \exp(-(t'-t)^2)/s) + n
#' \delta_{t}(t')} or Wiener process covariance \eqn{Cov(x(t), x(t')) =
#' \min(t',t)/s + n \delta_{t}(t')} with `scale` parameter s and `nugget` effect
#' n.
#'
#' @param n how many realizations to draw
#' @param grid vector of evaluation points (`argvals` of the return object).
#'   Defaults to (0, 0.02, 0.04, ..., 1).
#' @param scale scale parameter (see Description). Defaults to the width of the
#'   domain divided by 10.
#' @param cor type of correlation structure to use. Currently available:
#'   "squareexp" or "wiener".
#' @param nugget nugget effect for additional white error noise. Defaults to
#'   `scale/200`
#' @return an `feval`-vector of length `n`
#' @importFrom mvtnorm rmvnorm
#' @export
rgp <- function(n, grid = seq(0, 1, l = 51), scale = diff(range(grid))/10, 
  cor = c("squareexp", "wiener"), nugget = scale/200) {
  cor <- match.arg(cor)
  f_cov <- switch(cor, "wiener" = function(s, t) pmin(s, t)/scale,
    "squareexp" = function(s,t) exp(-(s - t)^2/scale))
  cov <- outer(grid, grid, f_cov) + diag(0*grid + nugget)
  y <- rmvnorm(n, mean = 0 * grid, sigma = cov)
  feval(y, argvals = grid)
}


