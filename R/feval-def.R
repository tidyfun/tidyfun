#' @import memoise
#' @import purrr
#' @import dplyr
new_feval <- function(argvals, datalist, regular, domain, range, evaluator, signif = 4) {
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
  range <- range %||% range(datalist, na.rm = TRUE)
  names(ret) <- names(ret) %||% seq_along(ret)
  structure(ret, 
    argvals =  argvals,
    domain = domain,
    range = range,
    evaluator = memoise(eval(evaluator)),
    evaluator_name = deparse(evaluator, width.cutoff = 60)[1],
    signif_argvals = signif, #maybe turn this into a <global> option? 
    class = c(class, "feval", "fvector"))
}

#------------------------------------------------------------------------------

#' Constructors for functional data evaluated on grids of argument values
#' 
#' Various constructor methods
#' 
#' @param data a `matrix`, `data.frame` or `list` of suitable shape.
#' @param ... not used
#' @return an `feval`-object
#' @export
feval <- function(data, ...) UseMethod("feval")

#' @export
#' @rdname feval
feval.matrix <- function(data, argvals = NULL, regular = NULL, domain = NULL, 
  range = NULL, evaluator = approx_linear, ...) {
  stopifnot(is.numeric(data))
  argvals <- find_argvals(data, argvals) # either arg or numeric colnames or 1:ncol
  names <- make.unique(rownames(data) %||% seq_len(dim(data)[1]))
  datalist <- split(data, names)
  regular <- regular %||% !any(is.na(data))
  new_feval(argvals, datalist, regular, domain, range, substitute(evaluator))
}
# default: use first 3 columns of <data> for function information

#' @export
#' @rdname feval
feval.data.frame <- function(data, id = 1, argvals = 2, value = 3, domain = NULL, 
  range = NULL, evaluator = approx_linear, ...) {
  data <- na.omit(data[, c(id, argvals, value)])
  stopifnot(is.numeric(data[[2]]), 
    is.numeric(data[[3]]))
  id <- data[[1]]
  datalist <- split(data[[3]], id)
  argvals <- split(data[[2]], id)
  regular <- sum(duplicated(argvals)) == length(argvals) - 1
  new_feval(argvals, datalist, regular, domain, range, substitute(evaluator))
}

# takes a list of vectors of identical lengths or a list of 2-column matrices/data.frames with 
# argvals in the first and data in the second column
#' @export
#' @rdname feval
feval.list <- function(data, argvals = NULL, regular = NULL, domain = NULL, 
  range = NULL, evaluator = approx_linear, ...) {
  vectors <- sapply(data, is.numeric)
  if (all(vectors)) {
    lengths <- sapply(data, length)
    if (all(lengths == lengths[1])) {
      data <- do.call(rbind, data)
      #dispatch to matrix method
      args <- list(data, argvals, regular, domain, range, 
        evaluator = substitute(evaluator))
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
  new_feval(argvals, data, regular, domain, range, substitute(evaluator))
}

