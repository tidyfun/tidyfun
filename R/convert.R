#' @rdname tfd
#' @export
as.tfd <- function(data, ...) UseMethod("as.tfd")
as.tfd.default <- function(data, ...) {
  tfd(data, ...)
}

# TODO: this ignores arg, domain for now, only needed internally in c.tfd
#' @rdname tfd
as.tfd_irreg <- function(data, ...) UseMethod("as.tfd_irreg")

as.tfd_irreg.tfd_reg <- function(data, ...) {
  arg <- ensure_list(tf_arg(data))
  ret <- map2(tf_evaluations(data), arg, ~list(arg = .y, value = .x))
  attributes(ret) <- attributes(data)
  attr(ret, "arg") <- numeric(0)
  class(ret)[1] <- "tfd_irreg"
  ret
}

as.tfd_irreg.tfd_irreg <- function(data, ...) {
  data
}

#' @rdname tfd
#' @param row.names not used
#' @param optional not used
#' @param x an `tfd` object
#' @inheritParams [.tf
#' @export
#' @importFrom tibble tibble
as.data.frame.tfd <- function(x, row.names = NULL, optional = FALSE,
                              arg = NULL, interpolate = FALSE, ...) {
  if (is.null(arg)) {
    arg <- ensure_list(tf_arg(x))
  }
  tmp <- x[, arg, interpolate = interpolate, matrix = FALSE]
  id <- unique_id(names(x)) %||% seq_along(x)
  id <- ordered(id, levels = id) # don't reshuffle
  tidyr::unnest(tibble::tibble(id = id, data = tmp), cols = data)
}

#' @rdname tfd
#' @export
as.matrix.tfd <- function(x, arg = NULL, interpolate = FALSE, ...) {
  if (is.null(arg)) {
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
#' @param row.names not used
#' @param optional not used
#' @param x an `tfb` object
#' @param arg optional vector of argument values
#' @export
as.data.frame.tfb <- function(x, row.names = NULL, optional = FALSE,
                              arg = NULL, ...) {
  if (is.null(arg)) {
    arg <- ensure_list(tf_arg(x))
  }
  tmp <- x[, arg, matrix = FALSE]
  id <- unique_id(names(x)) %||% seq_along(x)
  id <- ordered(id, levels = id) # don't reshuffle
  tidyr::unnest(tibble::tibble(id = id, data = tmp), cols = data)
}

#' @rdname tfb
as.matrix.tfb <- function(x, arg = NULL, ...) {
  # check arg-vector
  # rm dplyr / tidyr, evaluate basis then mutiply with coefs
  ret <- as.data.frame(x, arg = arg) %>%
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
