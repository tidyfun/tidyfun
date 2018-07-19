#' @import memoise
#' @import purrr
#' @import dplyr
new_tfd <- function(arg, datalist, regular, domain, evaluator, signif = 4) {
  assert_function(eval(evaluator))
  assert_set_equal(names(formals(eval(evaluator))), 
    c("x", "arg", "evaluations")) 
  arg_o <- map(arg, order)
  arg <- map2(arg, arg_o, ~.x[.y])
  datalist <- map2(datalist, arg_o, ~ unname(.x[.y]))
  domain <- signif(domain %||% range(arg, na.rm = TRUE), signif)
  if (!regular) {
    datalist <- map2(datalist, arg, 
      ~ list(arg = unname(signif(.y[!is.na(.x)], signif)), 
          value = unname(.x[!is.na(.x)])))
    arg <- numeric(0)
    class <- "tfd_irreg"
  } else {
    arg <- list(signif(arg[[1]], signif))
    class <- "tfd_reg"
  }
  ret <- structure(datalist, 
    arg =  arg,
    domain = domain,
    evaluator = memoise(eval(evaluator)),
    evaluator_name = deparse(evaluator, width.cutoff = 60)[1],
    signif_arg = signif, #maybe turn this into a <global> option? 
    class = c(class, "tfd", "tf"))
  assert_arg(arg, ret)
  ret
}

#------------------------------------------------------------------------------

#' Constructors for functional data evaluated on grids of argument values
#' 
#' Various constructor and conversion methods.
#' 
#' **`evaluator`**: must be a `function(x, arg, evaluations)` that returns
#' the function's (approximated/interpolated) values at locations `x` based on
#' the `evaluations` available at locations `arg`. 
#' Available `evaluator`-functions: 
#' 
#' - `approx_linear` for linear interpolation without extrapolation (i.e.,
#' [zoo::na.approx()] with `na.rm = FALSE`)  -- this is the default,
#' - `approx_spline` for cubic spline interpolation, (i.e., [zoo::na.spline()]
#' with `na.rm = FALSE`),
#' - `approx_fill_extend` for linear interpolation and constant extrapolation
#' (i.e., [zoo::na.fill()] with `fill = "extend"`)
#' - `approx_locf` for "last observation carried forward"  (i.e.,
#' [zoo::na.locf()] with `na.rm = FALSE` and
#' - `approx_nocb` for "next observation carried backward" (i.e.,
#' [zoo::na.locf()] with `na.rm = FALSE, fromLast = TRUE`).
#' 
#' See `tidyfun:::zoo_wrapper` and `tidyfun:::approx_linear`, which is simply
#' `zoo_wrapper(zoo::na.approx, na.rm = FALSE)`, for examples of implementations of
#' this. 
#' 
#' 
#' **`signif`**: `arg` that are equivalent to this significant digit are 
#' treated as identical. E.g., if an evaluation of f(t) is available at t=1 and a function
#' value is requested at t = 1.001, f(1) will be returned if `signif` < 4.
#' 
#' @param data a `matrix`, `data.frame` or `list` of suitable shape, or another `tf`-object.
#' @param ... not used in `tfd`, except for `tfd.tf` -- specify `arg` and `ìnterpolate = TRUE` to 
#'   turn an irregular `tfd` into a regular one, see examples. 
#' @return an `tfd`-object (or a `data.frame`/`matrix` for the conversion functions, obviously.)
#' @export
tfd <- function(data, ...) UseMethod("tfd")

#' @export
#' @rdname tfd
#' @param arg `numeric`, or list of `numeric`s. The evaluation grid. See Details.
#'  For the `data.frame`-methods: the name/number of the column defining the evaluation grid.
#' @param domain range of the `arg`. 
#' @param evaluator a function accepting arguments `x, arg, evaluations`. See details.
#' @param signif significant digits of the "resolution" of the evaluation grid.  See details. 
tfd.matrix <- function(data, arg = NULL, domain = NULL, 
    evaluator = approx_linear, signif = 4, ...) {
  stopifnot(is.numeric(data))
  arg <- find_arg(data, arg) # either arg or numeric colnames or 1:ncol
  id <- unique_id(rownames(data) %||% seq_len(dim(data)[1]))
  # make factor conversion explicit to avoid reordering
  datalist <- split(data, factor(id, unique(as.character(id))))
  names(datalist) <- rownames(data)
  regular <- !any(is.na(data))
  new_tfd(arg, datalist, regular, domain, substitute(evaluator), signif)
}
#' @rdname tfd
#' @export
tfd.numeric <- function(data, arg = NULL, 
    domain = NULL, evaluator = approx_linear, signif = 4, ...) {
  data <- t(as.matrix(data))
  #dispatch to matrix method
  args <- list(data, arg = arg, domain = domain,   
    evaluator = substitute(evaluator), signif = signif)
  return(do.call(tfd, args))
}

# default: use first 3 columns of <data> for function information
#' @export
#' @rdname tfd
#' @param id The name/number of the column defining which data belong to which function.
#' @param value The name/number of the column containing the function evaluations.
tfd.data.frame <- function(data, id = 1, arg = 2, value = 3, domain = NULL, 
    evaluator = approx_linear, signif = 4, ...) {
  data <- na.omit(data[, c(id, arg, value)])
  stopifnot(is.numeric(data[[2]]), 
    is.numeric(data[[3]]))
  # make factor conversion explicit to avoid reordering
  id <- factor(data[[1]], levels = as.factor(unique(data[[1]])))
  datalist <- split(data[[3]], id)
  arg <- split(data[[2]], id)
  regular <- length(arg) == 1 | all(duplicated(arg)[-1])
  new_tfd(arg, datalist, regular, domain, substitute(evaluator), signif)
}

# TODO this will break for multivariate data!
#' @description `tfd.list` accepts a list of vectors of identical lengths 
#' containing evaluations or a list of 2-column matrices/data.frames with 
#' `arg` in the first and evaluations in the second column
#' @export
#' @rdname tfd
tfd.list <- function(data, arg = NULL, domain = NULL, 
    evaluator = approx_linear, signif = 4, ...) {
  vectors <- map_lgl(data, ~ is.numeric(.) & !is.array(.)) 
  if (all(vectors)) {
    lengths <- sapply(data, length)
    if (all(lengths == lengths[1])) {
      data <- do.call(rbind, data)
      #dispatch to matrix method
      args <- list(data, arg = arg, domain = domain,   
        evaluator = substitute(evaluator), signif = signif)
      return(do.call(tfd, args))
    } else {
      stopifnot(!is.null(arg), length(arg) == length(data), 
        all(sapply(arg, length) == lengths))
      regular <- FALSE
    }
  }
  if (!any(vectors)) {
    dims <- map(data, dim)
    stopifnot(all(sapply(dims, length) == 2), all(map(dims, ~.x[2]) == 2),
      all(rapply(data, is.numeric)))
    arg <- map(data, ~ unlist(.x[, 1]))
    data <- map(data, ~ unlist(.x[, 2]))
    regular <- (length(data) == 1 | all(duplicated(arg)[-1])) 
  }
  new_tfd(arg, data, regular = regular, domain = domain, 
    evaluator = substitute(evaluator), signif = signif)
}

#' @export
#' @examples 
#' #turn irregular to regular tfd
#' #TODO: add extra function/verb for this
#' (f <- c(rgp(1, arg = seq(0,1,l=11)), rgp(1, arg = seq(0,1,l=21))))
#' tfd(f, interpolate = TRUE, arg = seq(0,1,l=21))
#' @rdname tfd
tfd.tf <- function(data, arg = NULL, domain = NULL, 
    evaluator = approx_linear, signif = 4, ...) {
  arg <- ensure_list(arg %||% arg(data))
  evaluations <- evaluate(data, arg)
  domain <- domain %||% domain(data)
  data <- as.data.frame(data, arg, ...)
  new_tfd(arg, evaluations, regular = (length(arg) == 1),
    domain = domain, evaluator = substitute(evaluator), 
    signif = signif)
}

