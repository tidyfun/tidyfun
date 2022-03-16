#' @rdname tfd
#' @export
as.tfd <- function(data, ...) UseMethod("as.tfd")
#' @export
as.tfd.default <- function(data, ...) {
  tfd(data, ...)
}

# TODO: this ignores arg, domain for now, only needed internally in c.tfd
#' @rdname tfd
#' @export
as.tfd_irreg <- function(data, ...) UseMethod("as.tfd_irreg")
#' @export
as.tfd_irreg.tfd_reg <- function(data, ...) {
  arg <- ensure_list(tf_arg(data))
  ret <- map2(tf_evaluations(data), arg, ~list(arg = .y, value = .x))
  attributes(ret) <- attributes(data)
  attr(ret, "arg") <- numeric(0)
  class(ret)[1] <- "tfd_irreg"
  ret
}
#' @export
as.tfd_irreg.tfd_irreg <- function(data, ...) {
  data
}

#' @rdname tfd
#' @inheritParams base::as.data.frame
#' @param optional not used!
#' @param x a `tf` object
#' @return a one-column `data.frame` with a `tf`-column containing `x`
#' @export
as.data.frame.tf <- function(x, row.names = NULL, optional = FALSE, ...) {
  colname <- deparse(substitute(x))
  ret <- data.frame(tmp = 1:length(x), row.names = row.names)
  ret[[colname]] <- x
  ret[, colname, drop = FALSE]
}
#as.data.frame.tfd <- as.data.frame.tf #TODO: y we need dis?
#as.data.frame.tfb <- as.data.frame.tf


#' @rdname tfd
#' @inheritParams [.tf
#' @export
as.matrix.tfd <- function(x, arg, interpolate = FALSE, ...) {
  if (missing(arg)) {
    arg <- sort(unique(unlist(tf_arg(x))))
  }
  ret <- x[, arg, interpolate = interpolate, matrix = TRUE]
  structure(ret, arg = as.numeric(colnames(ret)))
}

#-------------------------------------------------------------------------------

#' @rdname tfb
#' @export
as.tfb <- function(data, basis = c("spline", "fpc"), ...) tfb(data, basis, ...)

#' @rdname tfb
#' @param x a [tfb] object to be converted
#' @param arg a grid of argument values to evaluate on
#' @export
as.matrix.tfb <- function(x, arg, ...) {
  if (missing(arg)) arg <- tf_arg(x)
  assert_arg_vector(arg, x)
  # rm dplyr / tidyr, evaluate basis then mutiply with coefs
  ret <- tf_unnest(x, arg = arg) %>%
    dplyr::arrange(arg) %>%
    tidyr::spread(key = arg, value = value) %>%
    dplyr::select(-id) %>%
    as.matrix()
  rownames(ret) <- names(x)
  structure(ret, arg = as.numeric(colnames(ret)))
}

#-------------------------------------------------------------------------------
#' @export
as.function.tf <- function(x, ...) {
  function(arg) unlist(tf_evaluate(object = x, arg = arg))
}
