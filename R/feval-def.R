#' @import purrr
#' @import dplyr
feval <- function(data, ...) UseMethod("feval")

new_feval <- function(argvals, datalist, regular, domain, range) {
  if (!regular) {
    argvals <- map2(datalist, argvals, ~ .y[!is.na(.x)])
    datalist <- map(datalist, ~ .x[!is.na(.x)])
    class <- "feval_irreg"
  } 
  # TODO: add interpolation options e
  ret <- map2(argvals, datalist, ~ make_fapprox(.x, .y))
  if (regular) {
    argval_env <- new.env(); argval_env$.argvals <- argvals[[1]]
    ret <- map(ret, function(f) {
      rm(".argvals", envir = environment(f))
      parent.env(environment(f)) <- argval_env
      f
    })
    class <- "feval_reg"
  }
  domain <- domain %||% range(argvals)
  range <- range %||% range(datalist, na.rm = TRUE)
  names(ret) <- names(datalist) %||% seq_along(ret)
  structure(ret, 
    domain = domain,
    range = range,
    class = c(class, "feval", "fvector"))
}

feval.matrix <- function(data, argvals = NULL, regular = NULL, domain = NULL, 
  range = NULL, ...) {
  stopifnot(is.numeric(data))
  
  argvals <- find_argvals(data, argvals) # either arg or numeric colnames or 1:ncol
  datalist <- split(data, rownames(data) %||% seq_len(dim(data)[1]))
  regular <- regular %||% !any(is.na(data))
  new_feval(argvals, datalist, regular, domain, range)
}
# use first 3 columns of data for function information
feval.data.frame <- function(data, id = 1, argvals = 2, value = 3, domain = NULL, 
  range = NULL, ...) {
  stopifnot(ncol(data) >= 3, is.numeric(data[[argvals]]), 
    is.numeric(data[[value]]))
  datalist <- split(data[[value]], data[[id]])
  argvals <- split(data[[argvals]], data[[id]])
  regular <- sum(duplicated(argvals)) == length(argvals) - 1
  new_feval(argvals, datalist, regular, domain, range)
}
# takes a list of vectors of identical lengths or a list of 2-column matrices/data.frames with 
# argvals in the first and data in the second column
feval.list <- function(data, argvals = NULL, regular = NULL, domain = NULL, 
  range = NULL, ...) {
  vectors <- sapply(data, is.vector)
  if (all(vectors)) {
    lengths <- sapply(data, length)
    if (all(lengths == lengths[1])) {
      data <- do.call(rbind, data)
      #dispatch to matrix method
      return(feval(data, argvals, regular, domain, range, ...))
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
    id <- names(data) %||% seq_along(data)
    argvals <- map(data, ~ unlist(.x[, 1]))
    data <- map(data, ~ unlist(.x[, 2]))
    regular <- regular %||% all(duplicated(argvals))
  }
  new_feval(argvals, data, regular, domain, range)
}
