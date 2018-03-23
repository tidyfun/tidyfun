string_rep_fvector <- function(argvals, evaluations, signif_argvals = NULL, 
    show = 3, digits = NULL, ...) {
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

#-------------------------------------------------------------------------------

#' Pretty printing and formatting for functional data
#' 
#' Print/format `fvector`-objects.
#' 
#' @rdname fvectordisplay
#' @param n how many elements of `x` to print out
print.fvector <- function(x, n  = 10, ...) {
  cat(paste0("fvector[",length(x),"] on (", domain(x)[1], ",",
    domain(x)[2], ")"))
  invisible(x)
}

#' @rdname fvectordisplay
#' @export
print.feval_reg <- function(x, n = 10, ...) {
  NextMethod()
  cat(" based on", length(argvals(x)), "evaluations each\n")
  cat("interpolation by", attr(x, "evaluator_name"), "\n")
  if (length(x)) {
    cat(format(x[1:min(n, length(x))], ...), sep = "\n")
    if (n < length(x)) 
      cat(paste0("    [....]   (", length(x) - n, " not shown)\n"))
  }
  invisible(x)
}

#' @rdname fvectordisplay
#' @export
print.feval_irreg <- function(x, n = 10, ...) {
  NextMethod()
  nas <- map_lgl(evaluations(x), ~length(.)==1 && all(is.na(.)))
  n_evals <- n_evaluations(x[!nas])
  cat(paste0(" based on ", min(n_evals), " to ", max(n_evals)," (mean: ",
    round(mean(n_evals)),") evaluations each\n"))
  cat("inter-/extrapolation by", attr(x, "evaluator_name"), "\n")
  if (length(x)) {
    cat(format(x[1:min(n, length(x))], ...), sep = "\n")
    if (n < length(x)) 
      cat(paste0("    [....]   (", length(x) - n, " not shown)\n"))
  }  
  invisible(x)
}

#' @rdname fvectordisplay
#' @export
print.fbase <- function(x, n = 10, ...) {
  NextMethod()
  cat(" in basis representation:\n using basis ", attr(x, "basis_label"), "\n")
  if (length(x)) {
    cat(format(x[1:min(n, length(x))], ...), sep = "\n")
    if (n < length(x)) 
      cat(paste0("    [....]   (", length(x) - n, " not shown)\n"))
    invisible(x)
  }  
}

# FIXME: this needs proper width align etc arguments like format.default
#' @rdname fvectordisplay
#' @inheritParams base::format.default
#' @export
format.fvector <- function(x, digits = 2, nsmall = 0, ...){
  str <- map2_chr(ensure_list(argvals(x)), evaluations(x), string_rep_fvector, 
    signif_argvals = attr(x, "signif_argvals"), 
    digits = digits, nsmall = nsmall, ... = ...)
  if (is.null(names(x))) {
    str
  } else {
    map2_chr(names(x)[1:length(str)], str, ~ paste0(.x,": ",.y))
  }  
}
