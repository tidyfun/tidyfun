#-------------------------------------------------------------------------------

## tibble methods:
# adapted from https://github.com/r-spatial/sf/blob/master/R/tidyverse.R

#' Format tidy functional data for tibbles
#'
#' Summarize tidy functional data for tibble
#' @param x object of class tf
#' @param ... ignored
#' @rdname tftibble
#' @importFrom pillar type_sum obj_sum pillar_shaft
#' @details see [pillar::type_sum()]
type_sum.tf <- function(x, ...) {
  class(x)[2]
}

#' @rdname tftibble
obj_sum.tf <- function(x) {
  paste0(type_sum.tf(x), "[", length(x), "]")
}

#' @rdname tftibble
pillar_shaft.tf <- function(x, ...) {
  digits <- options("pillar.sigfig")$pillar.sigfig
  if (is.null(digits)) {
    digits <- options("digits")$digits
  }
  out <- format(x,
    width = 30L,
    digits = min(digits, abs(floor(log10(attr(x, "resolution"))))),
    prefix = FALSE, ...
  )
  pillar::new_pillar_shaft_simple(out, align = "right", min_width = NULL)
}
