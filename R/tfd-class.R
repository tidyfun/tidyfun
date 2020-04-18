#' @import memoise
#' @import purrr
#' @import dplyr
new_tfd <- function(arg, datalist, regular, domain, evaluator, resolution) {
  
  assert_string(evaluator)
  evaluator_f <- get(evaluator, mode = "function", envir = parent.frame())
  assert_function(evaluator_f)
  assert_set_equal(
    names(formals(evaluator_f)),
    c("x", "arg", "evaluations")
  )
  
  arg_o <- map(arg, order)
  arg <- map2(arg, arg_o, ~.x[.y])
  datalist <- map2(datalist, arg_o, ~unname(.x[.y]))
  domain <- domain %||% range(arg, na.rm = TRUE)
  resolution <- resolution %||% get_resolution(arg)
  
  if (!regular) {
    datalist <- map2(
      datalist, arg,
      ~list(
        arg = unname(.y[!is.na(.x)]),
        value = unname(.x[!is.na(.x)])
      )
    )
    n_evals <- map(datalist, ~length(.x$value))
    if (any(n_evals == 0)) warning("NA entries created.")
    datalist <- map_if(
      datalist, n_evals == 0,
      ~{
        list(arg = domain[1], value = NA)
      }
    )
    arg <- numeric(0)
    class <- "tfd_irreg"
  } else {
    arg <- list(arg[[1]])
    class <- "tfd_reg"
  }
  # ensure "unique" names (principles.tidyverse.org/names-attribute.html)
  names(datalist) <- names(datalist) %||% str_c("tf_", 1:length(datalist))
  
  ret <- vctrs::new_vctr(datalist,
    arg = arg,
    domain = domain,
    evaluator = memoise(evaluator_f),
    evaluator_name = evaluator,
    resolution = resolution, # maybe turn this into a <global> option?
    class = c(class, "tfd", "tf")
  )
  assert_arg(tf_arg(ret), ret)
  ret
}

#------------------------------------------------------------------------------

#' Constructors & converters for "raw" functional data
#'
#' Various constructor and conversion methods.
#'
#' **`evaluator`**: must be the (quoted or bare) name of a
#' `function(x, arg, evaluations)` that returns
#' the functions' (tf_approximated/interpolated) values at locations `x` based on
#' the `evaluations` available at locations `arg`.\cr
#' Available `evaluator`-functions:
#' - `tf_approx_linear` for linear interpolation without extrapolation (i.e.,
#' [zoo::na.approx()] with `na.rm = FALSE`)  -- this is the default,
#' - `tf_approx_spline` for cubic spline interpolation, (i.e., [zoo::na.spline()]
#' with `na.rm = FALSE`),
#' - `tf_approx_fill_extend` for linear interpolation and constant extrapolation
#' (i.e., [zoo::na.fill()] with `fill = "extend"`)
#' - `tf_approx_locf` for "last observation carried forward"  (i.e.,
#' [zoo::na.locf()] with `na.rm = FALSE` and
#' - `tf_approx_nocb` for "next observation carried backward" (i.e.,
#' [zoo::na.locf()] with `na.rm = FALSE, fromLast = TRUE`).
#' See `tidyfun:::zoo_wrapper` and `tidyfun:::tf_approx_linear`, which is simply
#' `zoo_wrapper(zoo::na.tf_approx, na.rm = FALSE)`, for examples of implementations of
#' this.
#'
#' **`resolution`**: `arg`-values that are equivalent up to this difference are
#' treated as identical. E.g., if an evaluation of $f(t)$ is available at $t=1$
#' and a function value is requested at $t = 1.01$, $f(1)$ will be returned if
#' `resolution` < .01. By default, resolution will be set to an integer-valued power
#' of 10 one smaller than smallest difference between adjacent
#' arg-values rounded down to an integer-valued power
#' of 10: e.g., if the smallest difference between consecutive
#' arg-values is between $0.1 and 0.9999$, the resolution will be $0.01$, etc.
#' In code: `10^(floor(log10(min(diff(<arg>))) - 1)`
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
#'  For the `data.frame`-method: the name/number of the column defining the evaluation grid.
#' @param domain range of the `arg`.
#' @param evaluator a function accepting arguments `x, arg, evaluations`. See details for [tfd()].
#' @param resolution resolution of the evaluation grid. See details for [tfd()].
tfd.matrix <- function(data, arg = NULL, domain = NULL,
                       evaluator = tf_approx_linear, resolution = NULL, ...) {
  stopifnot(is.numeric(data))
  evaluator <- quo_name(enexpr(evaluator))
  arg <- find_arg(data, arg) # either arg or numeric colnames or 1:ncol
  id <- unique_id(rownames(data) %||% seq_len(dim(data)[1]))
  # make factor conversion explicit to avoid reordering
  datalist <- split(data, factor(id, unique(as.character(id))))
  names(datalist) <- rownames(data)
  regular <- !any(is.na(data))
  new_tfd(arg, datalist, regular, domain, evaluator, resolution)
}
#' @rdname tfd
#' @export
tfd.numeric <- function(data, arg = NULL,
                        domain = NULL, evaluator = tf_approx_linear, 
                        resolution = NULL, ...) {
  evaluator <- quo_name(enexpr(evaluator))
  data <- t(as.matrix(data))
  # dispatch to matrix method
  args <- list(data,
    arg = arg, domain = domain,
    evaluator = evaluator, resolution = resolution
  )
  return(do.call(tfd, args))
}

#' @description `tfd.data.frame` uses the first 3 columns of <data> for function information by default:
#' (`ìd`, `arg`, `value`)
#' @export
#' @rdname tfd
#' @param id The name/number of the column defining which data belong to which function.
#' @param value The name/number of the column containing the function evaluations.
tfd.data.frame <- function(data, id = 1, arg = 2, value = 3, domain = NULL,
                           evaluator = tf_approx_linear, 
                           resolution = NULL, ...) {
  evaluator <- quo_name(enexpr(evaluator))
  data <- na.omit(data[, c(id, arg, value)])
  stopifnot(
    is.numeric(data[[2]]),
    is.numeric(data[[3]])
  )
  # make factor conversion explicit to avoid reordering
  id <- factor(data[[1]], levels = as.factor(unique(data[[1]])))
  datalist <- split(data[[3]], id)
  arg <- split(data[[2]], id)
  regular <- length(arg) == 1 | all(duplicated(arg)[-1])
  new_tfd(arg, datalist, regular, domain, evaluator, resolution)
}

# TODO this will break for multivariate data!
#' @description `tfd.list` accepts a list of vectors of identical lengths
#' containing evaluations or a list of 2-column matrices/data.frames with
#' `arg` in the first and evaluations in the second column
#' @export
#' @rdname tfd
tfd.list <- function(data, arg = NULL, domain = NULL,
                     evaluator = tf_approx_linear, resolution = NULL, ...) {
  evaluator <- quo_name(enexpr(evaluator))
  vectors <- map_lgl(data, ~ is.numeric(.) & !is.array(.))
  if (all(vectors)) {
    where_na <- map(data, is.na)
    data <- map2(data, where_na, ~ .x[!.y])
    lengths <- vapply(data, length, numeric(1))
    regular <- all(lengths == lengths[1]) &
      (is.numeric(arg) || all(duplicated(arg)[-1])) # duplicated(NULL) == TRUE!
    if (!regular) {
      stopifnot(!is.null(arg), length(arg) == length(data), 
                all(vapply(arg, length, numeric(1)) == 
                      vapply(where_na, length, numeric(1))))
      arg <- map2(arg, where_na, ~ .x[!.y])
    } else {
      if (!is.null(arg)) {
        arg <- ensure_list(arg)
        assert_numeric(arg[[1]], finite = TRUE, any.missing = FALSE, 
                       sorted = TRUE)
      }
    }
    
  }
  if (!any(vectors)) {
    dims <- map(data, dim)
    stopifnot(
      all(vapply(dims, length, numeric(1)) == 2), all(map(dims, ~.x[2]) == 2),
      all(rapply(data, is.numeric))
    )
    arg <- map(data, ~unlist(.x[, 1]))
    data <- map(data, ~unlist(.x[, 2]))
    regular <- (length(data) == 1 | all(duplicated(arg)[-1]))
  }
  new_tfd(arg, data,
    regular = regular, domain = domain,
    evaluator = evaluator, resolution = resolution
  )
}

#' @export
#' @examples
#' #turn irregular to regular tfd
#' #TODO: add extra function/verb for this
#' (f <- c(tf_rgp(1, arg = seq(0,1,l=11)), tf_rgp(1, arg = seq(0,1,l=21))))
#' tfd(f, interpolate = TRUE, arg = seq(0,1,l=21))
#' @rdname tfd
tfd.tf <- function(data, arg = NULL, domain = NULL,
                   evaluator = NULL, resolution = NULL, ...) {
  evaluator_name <- enexpr(evaluator)
  evaluator <- if (is_tfd(data) & is.null(evaluator)) {
    attr(data, "evaluator_name")
  } else {
    if (is.null(evaluator)) "tf_approx_linear" else quo_name(evaluator_name)
  }
  domain <- (domain %||% unlist(arg) %||% tf_domain(data)) %>% range()
  resolution <- resolution %||% tf_resolution(data)
  re_eval <- !is.null(arg)
  arg <- ensure_list(arg %||% tf_arg(data))
  evaluations <- if (re_eval) {
    tf_evaluate(data, arg) 
    } else {
      tidyfun::tf_evaluations(data)
    }
  if (re_eval) {
    nas <- map(evaluations, ~which(is.na(.x)))
    if (length(unlist(nas))) {
      warning(length(unlist(nas)), 
              " evaluations were NA, returning irregular tfd.")
      evaluations <- map2(evaluations, nas, ~{
        if (length(.y)) {
          .x[-.y]
        } else {
          .x
        }
      })
      arg <- map2(arg, nas, ~{
        if (length(.y)) {
          .x[-.y]
        } else {
          .x
        }
      })
    }
  }
  names(evaluations) <- names(data)
  new_tfd(arg, evaluations,
    regular = (length(arg) == 1),
    domain = domain, evaluator = evaluator, resolution = resolution
  )
}
