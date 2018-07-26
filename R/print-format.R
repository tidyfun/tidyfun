string_rep_tf <- function(f, signif_arg = NULL, 
    show = 3, digits = NULL, ...) {
  digits_eval <- digits %||% options()$digits
  digits_arg <- min(digits_eval, signif_arg %||% digits_eval) 
  arg_len <- map(ensure_list(arg(f)), length)
  show <- as.list(pmin(show, unlist(arg_len)))
  # fix weird dots handling by map/format:
  nsmall <- list(...)$nsmall %||% 0
  justify <- list(...)$justified %||% "right"
  arg_ch <- map2(ensure_list(arg(f)), show,
    ~ format(x = .x[1:.y], trim = FALSE, digits = digits_arg, 
      nsmall = nsmall, justify = justify, width = 4, ...))
  value_ch <- map2(evaluations(f), show,
    ~ format(x = .x[1:.y],  trim = FALSE, digits = digits_eval, 
      nsmall = nsmall, justify = justify, width = 4, ...))
  arg_nchar <- map(arg_ch, ~ nchar(.x)) %>% unlist %>% max
  value_nchar <- map(value_ch, ~ nchar(.x)) %>% unlist %>% max
  arg_ch <- map(arg_ch, ~stringr::str_pad(.x, arg_nchar))
  value_ch <- map(value_ch, ~stringr::str_pad(.x, value_nchar))
  str <- map2(arg_ch, value_ch, 
    ~ paste(paste0("(", .x, ",", .y, ")"), collapse = ";"))
  pmap(list(str, arg_len, show), 
    ~ ifelse(..2 > ..3, paste0(..1, "; ..."), ..1))
}

#-------------------------------------------------------------------------------

#' Pretty printing and formatting for functional data
#' 
#' Print/format `tf`-objects.
#' 
#' @rdname tfdisplay
#' @param n how many elements of `x` to print out
#' @export
print.tf <- function(x, n  = 10, ...) {
  cat(paste0(type_sum(x),"[",length(x),"] on (", domain(x)[1], ",",
    domain(x)[2], ")"))
  invisible(x)
}

#' @rdname tfdisplay
#' @export
print.tfd_reg <- function(x, n = 10, ...) {
  NextMethod()
  cat(" based on", length(arg(x)), "evaluations each\n")
  cat("interpolation by", attr(x, "evaluator_name"), "\n")
  if (length(x)) {
    cat(format(x[1:min(n, length(x))], ...), sep = "\n")
    if (n < length(x)) 
      cat(paste0("    [....]   (", length(x) - n, " not shown)\n"))
  }
  invisible(x)
}

#' @rdname tfdisplay
#' @export
print.tfd_irreg <- function(x, n = 10, ...) {
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

#' @rdname tfdisplay
#' @export
print.tfb <- function(x, n = 10, ...) {
  NextMethod()
  cat(" in basis representation:\n using basis ", attr(x, "basis_label"), "\n")
  if (length(x)) {
    cat(format(x[1:min(n, length(x))], ...), sep = "\n")
    if (n < length(x)) 
      cat(paste0("    [....]   (", length(x) - n, " not shown)\n"))
    invisible(x)
  }  
}

#' @rdname tfdisplay
#' @inheritParams base::format.default
#' @param prefix used internally.
#' @export
format.tf <- function(x, digits = 2, nsmall = 0, width = options()$width, 
    n  = 10, prefix = TRUE, ...) {
  long <- length(x) > n
  if (long && width > 0 && width <= 30) 
    x = head(x, n)
  str <- string_rep_tf(x, signif_arg = attr(x, "signif_arg"), 
    digits = digits, nsmall = nsmall, ...)
  if (prefix) {
    prefix <- if (!is.null(names(x))) 
      names(x)[1:length(str)] else paste0("[", 1:length(str), "]")
    str <- map2(prefix, str, ~ paste0(.x,": ",.y))
  }  
  unlist(map_if(str, ~ nchar(.x) > width, 
    ~ paste0(substr(.x, 1, width - 3), "...")))
}

#-------------------------------------------------------------------------------

## tibble methods:
# adapted from https://github.com/r-spatial/sf/blob/master/R/tidyverse.R 

#' Summarize tidy functional data for tibble
#'
#' Summarize tidy functional data for tibble
#' @param x object of class tf
#' @param ... ignored
#' @rdname tftibble
#' @importFrom pillar type_sum obj_sum pillar_shaft
#' @details see [pillar::type_sum()]
type_sum.tf <- function(x, ...) {
  class(x)[length(class(x)) - 1]
}

#' @rdname tftibble
obj_sum.tf <- function(x) {
  paste0(type_sum.tf(x), "[", length(x), "]")
}

#' @rdname tftibble
pillar_shaft.tf <- function(x, ...) {
  digits = options("pillar.sigfig")$pillar.sigfig
  if (is.null(digits))
    digits = options("digits")$digits
  out <- format(x, width = 30L, digits = min(digits, attr(x, "signif")), 
    prefix = FALSE, ...)
  pillar::new_pillar_shaft_simple(out, align = "right", min_width = 25)
}
