#' @import purrr
#' @import dplyr
feval <- function(data, ...) UseMethod("feval")

new_feval <- function(argvals, datalist, regular, domain, range) {
  if (!regular) {
    argvals <- map2(datalist, argvals, ~ .y[!is.na(.x)])
    datalist <- map(datalist, ~ .x[!is.na(.x)])
    class <- "feval_irreg"
  } 
  ret <- map2(argvals, datalist, ~ make_fapprox(.x, .y))
  if (regular) {
    argval_env <- new.env(); argval_env$argvals <- argvals[[1]]
    ret <- map(ret, function(f) {
      rm("argvals", envir = environment(f))
      parent.env(environment(f)) <- argval_env
      f
    })
    class <- "feval_reg"
  }
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
  
  domain <- domain %||% range(argvals)
  range <- range %||% range(data, na.rm = TRUE)
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
  domain <- domain %||% range(argvals)
  range <- range %||% range(datalist, na.rm = TRUE)
  regular <- sum(duplicated(argvals)) == length(argvals) - 1
  new_feval(argvals, datalist, regular, domain, range)
}
