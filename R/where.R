#' Find out where functional data fulfills certain conditions.
#'
#' `tf_where` allows to define a logical expression about the function values
#' and returns the argument values for which that condition is true.\cr
#' `tf_anywhere` is syntactic sugar for `tf_where` with `return = "any"` to
#' get a logical flag for each function if the condition is `TRUE` *anywhere*,
#' see below.
#'
#' Entries in `f` that do not fulfill `cond` anywhere yield `numeric(0)`.\cr
#' `cond`  is evaluated as a [dplyr::filter()]-statement on a `data.frame`
#' containing a single entry in `f` with columns `arg` and `value`, so all
#' the usual `dplyr` tricks are available, see examples.\cr
#' Any condition evaluates to `NA` on `NA`-entries in `f`.
#'
#' @param f a `tf` object
#' @param cond a logical expression on `value` that defines the condition about
#'   the function values, see examples and details.
#' @param return for each entry in `f`, `tf_where` either returns *all* `arg` for
#'   which `cond` is true, the *first*, the *last* or their *range* or logical
#'   flags whether the functions fullfill the condition *any*where. For
#'   `"range"`, note that `cond` may not be true for all `arg` values in this
#'   range, though, this is not checked.
#' @param arg optional `arg`-values on which to evaluate `f` and check `cond`,
#'   defaults to `arg(f)`.
#' @return depends on  `return`:  
#'  - `return = "any"`, i.e, `anywhere`: a logical vector of the same length as `f`.
#'  - `return = "all"`: a list of vectors of the same length as `f`, with  
#'     empty vectors for the functions that  never fulfill the `cond`ition.
#'  - `return = "range"`: a data frame with columns "begin" and "end".
#'  - else, a numeric vector of the same length as `f` with `NA`s for the functions that  never fulfill the `cond`ition.
#' @examples 
#'   lin <- 1:4 * tfd(seq(-1, 1,l = 11), seq(-1, 1, l = 11))
#'   tf_where(lin, value %inr% c(-1, .5))
#'   tf_where(lin, value %inr% c(-1, .5), "range")
#'   a <- 1
#'   tf_where(lin, value > a, "first")
#'   tf_where(lin, value < a, "last")
#'   tf_where(lin, value > 2, "any")
#'   anywhere(lin, value > 2)
#' 
#'   set.seed(4353)
#'   plot(f <- rgp(5, 11L), pch = as.character(1:5), points = TRUE)
#'   tf_where(f, value == max(value))
#'   # where is the function increasing/decreasing:
#'   tf_where(f, value > dplyr::lag(value, 1, value[1]))
#'   tf_where(f, value < dplyr::lead(value, 1, value[n()]))
#'   # where are the (interior) extreme points:
#'   tf_where(f, 
#'     sign(c(diff(value)[1], diff(value))) !=
#'       sign(c(diff(value), diff(value)[n()-1])))
#'   # where for arg > .5 is the function positive:
#'   tf_where(f, arg > .5 & value > 0)
#'   # does the function ever exceed 1:
#'   tf_anywhere(f, value > 1)
#' @export
tf_where <- function(f, cond, return = c("all", "first", "last", "range", "any"),
    arg) {
  if (missing(arg)) {
    arg <- tidyfun::arg(f)
  } else assert_arg(arg, f)
  return <- match.arg(return)
  cond <- enquo(cond)
  where_at <- map(f[, arg, matrix = FALSE], 
    ~ filter(.x, !! cond) %>% pull(arg))
  if (return == "first") {
    where_at <- map_if(where_at, ~ length(.x) > 0, min)
  }
  if (return == "last") {
    where_at <- map_if(where_at, ~ length(.x) > 0, max)
  }
  if (return == "range") {
    where_at <- map_if(where_at, ~ length(.x) > 0, range)
  }
  if (return == "any") {
    where_at <- map_lgl(where_at, ~ length(.x) > 0)
  }
  where_at[is.na(f)] <- NA
  if (return == "all") return(where_at)
  where_at <- map_if(where_at, ~ length(.x) == 0, ~ {NA})
  if (return == "range") {
    where_at <- map_if(where_at, ~ all(is.na(.x)), ~ {c(NA, NA)}) %>% 
      do.call(what = rbind, args = .) %>% as.data.frame %>% 
      rename(begin = V1, end = V2)
    return(where_at)
  } 
  unlist(where_at)
}
#' @rdname tf_where
#' @export
tf_anywhere <- function(f, cond, arg) {
  call <- match.call()
  call[[1]] <- tf_where
  call$return <- "any"
  eval(call, parent.frame())
}

#' @description `in_range` and its infix-equivalent return `TRUE` for all 
#'    values in `f` that are within the range of values in `r`. 
#' @param r used to specify a range, only the minimum and maximum of `r` are used.
#' @rdname tf_where
#' @export
in_range <- function(f, r){
  assert_numeric(f)
  assert_numeric(r)
  r <- range(r, na.rm = TRUE)
  f >= r[1] & f <= r[2]
}
#' @rdname tf_where
#' @export
`%inr%` <- function(f, r) in_range(f, r)

