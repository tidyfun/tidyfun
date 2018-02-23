#' @import memoise
#' @import purrr
#' @import dplyr
new_feval <- function(argvals, datalist, regular, domain,   evaluator, signif = 4) {
  if (!regular) {
    argvals <- map2(datalist, argvals, ~ signif(.y[!is.na(.x)], signif))
    ret <- map(datalist, ~ .x[!is.na(.x)])
    class <- "feval_irreg"
  } else {
    argvals <- list(signif(argvals[[1]], signif))
    ret <- datalist
    class <- "feval_reg"
  }
  domain <- domain %||% range(argvals)
  names(ret) <- names(ret) %||% seq_along(ret)
  ret <- structure(ret, 
    argvals =  argvals,
    domain = domain,
     
    evaluator = memoise(eval(evaluator)),
    evaluator_name = deparse(evaluator, width.cutoff = 60)[1],
    signif_argvals = signif, #maybe turn this into a <global> option? 
    class = c(class, "feval", "fvector"))
  assert_argvals(argvals, ret)
  ret
}

#------------------------------------------------------------------------------

#' Constructors for functional data evaluated on grids of argument values
#' 
#' Various constructor and conversion methods.
#' 
#' **`evaluator`**: must be a `function(x, argvals, evaluations)` that returns
#' the function's (approximated/interpolated) values at locations `x` based on
#' the `evaluations` available at locations `argvals`. 
#' Available `evaluator`-functions: 
#' 
#' - `approx_linear` for linear interpolation without extrapolation (i.e.,
#' [zoo::na.approx()] with `na.rm = FALSE`),
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
#' **`signif`**: `argvals` that are equivalent to this significant digit are 
#' treated as identical. E.g., if an evaluation of f(t) is available at t=1 and a function
#' value is requested at t = 1.001, f(1) will be returned if `signif` < 4.
#' 
#' @param data a `matrix`, `data.frame` or `list` of suitable shape, or another `fvector`-object.
#' @param ... not used
#' @return an `feval`-object (or a `data.frame`/`matrix` for the conversion functions, obviously.)
#' @export
feval <- function(data, ...) UseMethod("feval")

#' @export
#' @rdname feval
#' @param argvals `numeric`, or list of `numeric`s. The evaluation grid. See Details.
#' @param regular `logical` assume a regular or iregular grid
#' @param domain range of the `argvals`. 
#' @param evaluator a function accepting arguments `x, argvals, evaluations`. See details.
#' @param signif significant digits of the "resolution" of the evaluation grid.  See details. 
feval.matrix <- function(data, argvals = NULL, regular = NULL, domain = NULL, 
    evaluator = approx_linear, signif = 4, ...) {
  stopifnot(is.numeric(data))
  argvals <- find_argvals(data, argvals) # either arg or numeric colnames or 1:ncol
  names <- make.unique(rownames(data) %||% seq_len(dim(data)[1]))
  datalist <- split(data, names)
  regular <- regular %||% !any(is.na(data))
  new_feval(argvals, datalist, regular, domain,   substitute(evaluator), signif)
}

# default: use first 3 columns of <data> for function information
#' @export
#' @rdname feval
#' @param id The name/number of the column defining which data belong to which function.
#' @param value The name/number of the column containing the function evaluations.
feval.data.frame <- function(data, id = 1, argvals = 2, value = 3, domain = NULL, 
    evaluator = approx_linear, signif = 4, ...) {
  data <- na.omit(data[, c(id, argvals, value)])
  stopifnot(is.numeric(data[[2]]), 
    is.numeric(data[[3]]))
  id <- data[[1]]
  datalist <- split(data[[3]], id)
  argvals <- split(data[[2]], id)
  regular <- sum(duplicated(argvals)) == length(argvals) - 1
  new_feval(argvals, datalist, regular, domain,   substitute(evaluator), signif)
}

# takes a list of vectors of identical lengths or a list of 2-column matrices/data.frames with 
# argvals in the first and data in the second column
#' @export
#' @rdname feval
feval.list <- function(data, argvals = NULL, regular = NULL, domain = NULL, 
    evaluator = approx_linear, signif = 4, ...) {
  vectors <- sapply(data, is.numeric)
  if (all(vectors)) {
    lengths <- sapply(data, length)
    if (all(lengths == lengths[1])) {
      data <- do.call(rbind, data)
      #dispatch to matrix method
      args <- list(data, argvals, regular, domain,   
        evaluator = substitute(evaluator), signif)
      return(do.call(feval, args))
    } else {
      stopifnot(!is.null(argvals), length(argvals) == length(data), 
        all(sapply(argvals, length) == lengths))
      if (!is.null(regular) && regular) warning("data is not regular.")
      regular <- FALSE
    }
  }
  if (!any(vectors)) {
    dims <- map(data, dim)
    stopifnot(all(sapply(dims, length) == 2), all(map(dims, ~.x[2]) == 2),
      all(rapply(data, is.numeric)))
    id <- make.unique(names(data) %||% seq_along(data))
    argvals <- map(data, ~ unlist(.x[, 1]))
    data <- map(data, ~ unlist(.x[, 2]))
    names(data) <- id
    regular <- regular %||% all(duplicated(argvals))
  }
  new_feval(argvals, data, regular, domain,   substitute(evaluator), signif)
}

#' @export
feval.fvector <- function(data, argvals = NULL, regular = NULL, domain = NULL, 
    evaluator = approx_linear, signif = 4, ...) {
  argvals <- ensure_list(argvals %||% argvals(data))
  evaluations <- evaluate(data, argvals)
  domain <- domain %||% domain(data)
  data <- as.data.frame(data, argvals)
  new_feval(argvals, evaluations, regular = length(argvals) == 1,
    domain = domain,   evaluator = substitute(evaluator), 
    signif = signif)
}
