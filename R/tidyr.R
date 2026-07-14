#' Gather all columns representing functional measurements into a `tfd`-object
#'
#' Similar in spirit to [tidyr::gather()], but does NOT put the values in the
#' gathered columns into one very long "value"-column while labeling the different
#' original columns in a very long "key"-column -- instead it creates a `tfd`-column
#' containing the functional measurements of the columns given in `...`.
#'
#' @param data a data frame -- note that `dplyr` does not handle matrix columns well,
#'    if `data` contains more than one of those, `tf_gather` will fail...
#' @param ... A selection of columns to collect as a `tfd` object. Each column
#'   represents measurements of a functional variable at a specific `arg`-val.
#'   Can also be the name of a matrix-valued column, but see above.
#'   If empty, all variables are selected. You can supply bare variable names,
#'   select all variables between x and z with x:z, exclude y with -y. For more
#'   options, see the [dplyr::select()] documentation.
#' @param key the name of the created `tfd`-column. Defaults to `".tfd"`, but
#'   the function will try to guess the name based on the column names of the
#'   gathered columns in this case. If a common prefix of all column names
#'   is found, this is used instead. You also get a message about this.
#' @param arg optional. Argument values for the functions. If not provided, will be guessed from the column names as well.
#'   See also [tf::tfd()].
#' @param evaluator optional. A function accepting arguments x, arg, evaluations. See [tf::tfd()] for details.
#' @param domain optional. Range of possible `arg`-values. See [tf::tfd()] for details.
#' @returns a modified `data.frame` with a `tfd` column replacing the `...`.
#' @import dplyr
#' @export
#' @seealso [dplyr::select()]
#' @family tidyfun data wrangling functions
#' @examples
#' d <- tf_spread(growth[1:5, ]) |> dplyr::mutate(id = 1:dplyr::n())
#' dplyr::glimpse(d)
#' # tidyselect syntax for column selection:
#' tf_gather(d, starts_with("height"))
#' tf_gather(d, height_1:height_18)
#' tf_gather(d, -gender, -id)
#' # custom key name and arg values:
#' tf_gather(d, starts_with("height"), key = "height")
#' tf_gather(d, starts_with("height"), arg = seq(0, 1, length.out = 31))
tf_gather <- function(
  data,
  ...,
  key = ".tfd",
  arg = NULL,
  domain = NULL,
  evaluator = tf_approx_linear
) {
  key_var <- as_name(enexpr(key))
  search_key <- isTRUE(key == ".tfd")
  if (...length() == 0) {
    gather_vars <- names(data)
  } else {
    gather_vars <- names(eval_select(expr(c(...)), data))
  }
  if (is_empty(gather_vars)) {
    return(data)
  }
  # turn matrix column into regular columns:
  if (
    length(gather_vars) == 1 && is.matrix(data[[gather_vars]]) && search_key
  ) {
    key_var <- gather_vars
    search_key <- FALSE
    cli::cli_inform("creating new {.cls tfd}-column {.val {key_var}}")
  }

  tfd_data <- data |>
    select(all_of(gather_vars)) |>
    as.matrix()
  if (search_key) {
    # see also find_arg: will interpret separating-dashes as minus-signs
    # regex adapted from https://www.regular-expressions.info/floatingpoint.html
    found_key <- unique(sub(
      "[-+]?(0|(0\\.[0-9]+)|([1-9][0-9]*\\.?[0-9]*))([eE][-+]?[0-9]+)?$",
      "",
      colnames(tfd_data)
    ))
    # assume trailing 0's are padding:
    found_key <- sub("[0]+$", "", found_key)
    # assume trailing punctuation is separator:
    found_key <- sub("[[:punct:]]$", "", found_key)
    # check again for uniqueness of resulting value ...
    found_key <- unique(found_key)
    if (length(found_key) == 1 && all(found_key != "")) {
      key_var <- found_key
      cli::cli_inform("creating new {.cls tfd}-column {.val {key_var}}")
    }
  }

  data |>
    select(-all_of(gather_vars)) |>
    mutate(
      !!key_var := tfd(
        tfd_data,
        arg = arg,
        domain = domain,
        evaluator = {{ evaluator }}
      )
    )
}

#' Spread a `tf`-column into many columns representing the
#' function evaluations.
#'
#' Similar in spirit to [tidyr::spread()], but does NOT shorten,
#' just widens the data frame  -- a `tf`-column is spread out into many columns
#' containing the functional measurements.
#'
#' @param data a data frame with at least one `tf`-column
#' @param value the name of the `tf`-column to 'spread'/evaluate.
#'   You can supply bare variable names etc., see the [dplyr::select()]
#'   documentation. Also works without this if there's only one `tf` in `data`,
#'   see examples.
#' @param arg (Semi-)optional. A vector of `arg`-values on which to evaluate the
#'   functions. If not provided, uses the default `arg`s. Should be
#'   specified for `tf_irreg`, otherwise *all* observed gridpoints are used for
#'   *every* function.
#' @param sep separating character used to create column names for the new columns,
#'   defaults to `"_"` for column names "<name of the `tf`>_<`arg`-value>".
#'   Set to NULL to get column names that only contain the `arg`-value.
#' @param interpolate `interpolate`-argument for evaluating the functional data.
#'   Defaults to FALSE, i.e., `tfd`s are *not* inter/extrapolated on unobserved
#'    `arg`-values.
#' @returns a wider dataframe with the `tf`-column spread out into many columns
#'   each containing the functional measurements for one `arg`-value.
#' @importFrom tidyselect vars_pull
#' @export
#' @family tidyfun data wrangling functions
#' @examples
#' d <- dplyr::tibble(g = 1:3)
#' d$f <- tf_rgp(3, 11L)
#' tf_spread(d, f)
#' tf_spread(d, -g)
#' tf_spread(d)
tf_spread <- function(data, value, arg, sep = "_", interpolate = FALSE) {
  if (missing(value)) {
    tf_cols <- which(map_lgl(data, is_tf))
    if (length(tf_cols) == 0) {
      cli::cli_warn(
        "{.arg value} {.val {tf_var}} is not a column of class {.cls tf}. Nothing's happening here."
      )
      return(data)
    }
    if (length(tf_cols) == 1) {
      value <- tf_cols
    } else {
      cli::cli_abort(
        "More than one {.cls tf} found, specify which one to spread in {.arg value}."
      )
    }
  }
  tf_var <- vars_pull(names(data), !!enquo(value))
  tf <- data[[tf_var]]
  if (!is_tf(tf)) {
    cli::cli_warn(
      "{.arg value} {.val {tf_var}} is not a column of class {.cls tf}. Nothing's happening here."
    )
    return(data)
  }
  if (missing(arg)) {
    arg <- tf_arg(tf)
    if (is_irreg(tf)) {
      arg <- unlist(arg, use.names = TRUE) |> unique() |> sort()
      cli::cli_warn(
        "no explicit {.arg arg} for irregular {.val {tf_var}} provided -- using all {length(arg)}, distinct observed argument values.",
      )
    }
  }
  tf_eval <- tf[, arg, matrix = TRUE, interpolate = interpolate] |>
    as.data.frame()
  if (!is.null(sep)) {
    colnames(tf_eval) <- paste0(tf_var, sep, arg)
  }
  data |>
    select(-all_of(tf_var)) |>
    bind_cols(tf_eval)
}

# ------------------------------------------------------------------------------

#' Turn "long" tables into tidy data frames with `tf`-objects
#'
#' Similar in spirit to [tidyr::nest()]. This turns tables in "long" format,
#' where one column (`.id`) defines the unit of observation, one column (`.arg`)
#' defines the evaluation grids of the functional observations, and other columns (`...`)
#' define the values of the functions at those points into a (much shorter) table containing
#' `tfd`-objects. All other variables are checked for constancy over `.id` and
#' appended as well.
#'
#' `domain` and `evaluator` can be specified as lists or vectors
#' if you are nesting multiple functional data columns with different properties.
#' Because this interface captures evaluator names as text, supply the evaluator
#' as a string rather than a bare function name.
#'
#' @param data a data frame
#' @param ... A selection of columns. If empty, all variables except the
#'   `.id` and `.arg` columns are selected. You can supply bare variable names,
#'   select all variables between `x` and `z` with `x:z`, exclude `y` with `-y`.
#'   For more options, see the [dplyr::select()] documentation.
#' @param .id the (bare or quoted) name of the column defining the different
#'   observations. Defaults to "id".
#' @param .arg the (bare or quoted) name of the column defining the `arg`-values
#'   of the observed functions. Defaults to "arg".
#' @inheritParams tf_gather
#' @returns a data frame with (at least) `.id` and `tfd` columns
#' @export
#' @family tidyfun data wrangling functions
#' @seealso [tfd()] for details on `domain` and `evaluator`.
#' @examples
#' d <- dplyr::tibble(id = rep(1:3, each = 5), arg = rep(1:5, 3), value = rnorm(15))
#' tf_nest(d, .id = id, .arg = arg)
tf_nest <- function(
  data,
  ...,
  .id = "id",
  .arg = "arg",
  domain = NULL,
  evaluator = "tf_approx_linear"
) {
  if (!is.data.frame(data)) {
    cli::cli_abort(
      "{.arg data} must be data frame, not {.obj_type_friendly {data}}."
    )
  }
  if (inherits(data, "grouped_df")) {
    cli::cli_abort(c(
      "{.fn tf_nest} does not work for {.cls grouped_df}.",
      i = "{.fn ungroup} your data before nesting."
    ))
  }
  arg_var <- as_name(enexpr(.arg))
  id_var <- as_name(enexpr(.id))
  if (...length() == 0) {
    value_vars <- setdiff(names(data), c(id_var, arg_var))
  } else {
    value_vars <- names(eval_select(expr(c(...)), data))
  }
  n_value_vars <- length(value_vars)
  if (n_value_vars == 0) {
    return(data)
  }
  # homogenize inputs:
  if (!length(evaluator) %in% c(1, n_value_vars)) {
    cli::cli_abort(
      "{.arg evaluator} length must be 1 or {n_value_vars}, not {length(evaluator)}."
    )
  }
  if (!is.list(domain)) {
    domain <- replicate(n_value_vars, domain, simplify = FALSE)
  } else {
    if (!length(domain) %in% c(1, n_value_vars)) {
      cli::cli_abort(
        "{.arg domain} length must be 1 or {n_value_vars}, not {length(domain)}."
      )
    }
  }

  evaluator <- as.list(evaluator)
  if (!length(evaluator) %in% c(1, n_value_vars)) {
    cli::cli_abort(
      "{.arg evaluator} length must be 1 or {n_value_vars}, not {length(evaluator)}."
    )
  }

  remaining <- setdiff(names(data), c(id_var, arg_var, value_vars))
  # check that nesting is possible without information loss
  ret <- data |>
    select(all_of(c(id_var, remaining))) |>
    group_by(across(all_of(id_var)))
  not_constant <- ret |>
    summarise(across(everything(), n_distinct)) |>
    select(-all_of(id_var)) |>
    summarise(across(everything(), \(x) !all(x == 1L))) |>
    select(where(isTRUE))
  if (ncol(not_constant) > 0) {
    cli::cli_abort(
      "Can't nest - columns {.val {names(not_constant)}} are not constant for all levels of {.arg id}"
    )
  }

  # keep first line of every id-level:
  ret <- ret |> slice(1) |> ungroup()

  tfd_list <- pmap(
    list(value_vars, evaluator, domain),
    function(x, y, z) {
      data |>
        select(all_of(c(id_var, arg_var, x))) |>
        tfd(evaluator = !!y, domain = z)
    }
  )
  names(tfd_list) <- value_vars
  # re-index to make sure order is correct,
  # use <character> index so that numeric ids are interpreted correctly
  # as names (f["1000",]), not index positions (f[1000, ])
  id_index <- ret |> pull(id_var) |> as.character()
  for (v in value_vars) {
    ret[[v]] <- tfd_list[[v]][id_index, ]
  }
  ret
}

#-------------------------------------------------------------------------------

#' Turn (data frames with) `tf`-objects / list columns into "long" tables.
#'
#' Similar in spirit to [tidyr::unnest()], the reverse of [tf_nest()].
#' The `tf`-method simply turns a single `tfd` or `tfb` vector into a "long" [tibble::tibble()].
#' For a multivariate `tf_mv` vector (functions \eqn{R \to R^d}{R -> R^d}) the
#' `tf_mv`-method returns a "wide" long table with one value column per output
#' dimension: `(id, arg, <component 1>, ..., <component d>)`.
#'
#' - Caution: this uses slightly different defaults for names of unnested columns
#'   than `tidyr::unnest()`.
#' - For `data.frames`, include an ID column with a unique row identifier before
#'   unnesting. Without it, arg-value pairs cannot be matched back to their
#'   original functions after unnesting.
#'
#' @param data a data.frame or a `tf`-object
#' @param arg optional values for the `arg` argument of
#'   [tf::tf_evaluate()]
#' @param interpolate return function values for `arg`-values not on original grid?
#'   Defaults to `TRUE`.
#' @param ... not used currently
#' @inheritParams tidyr::unnest
#' @returns a "long" data frame with `tf`-columns expanded into `arg, value`-
#'   columns.
#' @seealso [tf_evaluate.data.frame()]
#' @export
#' @family tidyfun data wrangling functions
#' @examples
#' d <- dplyr::tibble(id = 1:3)
#' d$f <- tf_rgp(3, 11L)
#' tf_unnest(d, f)
tf_unnest <- function(data, cols, arg, interpolate = TRUE, ...) {
  UseMethod("tf_unnest")
}

#' @export
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @rdname tf_unnest
tf_unnest.tf <- function(data, cols, arg, interpolate = TRUE, ...) {
  if (missing(arg) || is.null(arg)) {
    arg <- tf::ensure_list(tf_arg(data))
  }
  tmp <- data[, arg, matrix = FALSE, interpolate = interpolate]
  id <- unique_id(names(data)) %||% seq_along(data)
  id <- ordered(id, levels = id) # don't reshuffle
  tidyr::unnest(tibble::tibble(id = id, data = tmp), cols = data)
}

#' @export
#' @importFrom purrr imap reduce
#' @rdname tf_unnest
tf_unnest.tf_mv <- function(data, cols, arg, interpolate = TRUE, ...) {
  # "wide" long form: (id, arg, <comp_1>, ..., <comp_d>).
  # Deliberately NOT tf::as.data.frame.tf_mv(unnest = TRUE, long = FALSE): that
  # evaluates components on per-curve *union* grids, interpolating each
  # component at the other components' args inside its observed range. Here each
  # component is unnested on its own grid (or `arg`) via tf_unnest.tf and
  # full-outer-joined on (id, arg), so a component is NA wherever it was not
  # actually observed/evaluated, and `id` stays the ordered factor from
  # tf_unnest.tf.
  if (missing(arg)) arg <- NULL
  comps <- tf_components(data)
  if (length(data) == 0L) {
    # tf_unnest.tf on zero-length components yields no (arg, value) columns to
    # join on -- return the empty wide schema directly
    empty <- c(
      list(id = ordered(character(0)), arg = numeric(0)),
      stats::setNames(
        rep(list(numeric(0)), length(comps)),
        names(comps)
      )
    )
    return(tibble::as_tibble(empty))
  }
  per <- imap(comps, function(comp, nm) {
    one <- tf_unnest(comp, arg = arg, interpolate = interpolate)
    names(one)[names(one) == "value"] <- nm
    one
  })
  out <- reduce(per, full_join, by = c("id", "arg"))
  out[order(out$id, out$arg), , drop = FALSE]
}

# Long form with one row per (id, arg, component):
# (id, arg, .component, value, .mv_group). `.component` is an ordered factor
# over the component names, `.mv_group` an ordered factor over id x component
# (one group per curve and component). Assembled component-by-component on
# each component's own grid -- NOT by pivoting the wide outer join: pivoting
# would create structural NA rows (component unobserved at another
# component's arg) that must be dropped, and dropping them also removes
# *genuine* missing evaluations, so geom_line() would connect straight
# across actually-missing intervals instead of breaking there. Per-component
# assembly produces no structural rows, and genuine NAs stay in.
# Used to drive value-vs-arg geoms.
.tf_mv_unnest_long <- function(data, arg = NULL, interpolate = TRUE) {
  comps <- tf_components(data)
  nms <- names(comps)
  if (length(data) == 0L || !length(comps)) {
    return(tibble::tibble(
      id = ordered(character(0)),
      arg = numeric(0),
      value = numeric(0),
      .component = ordered(character(0), levels = nms),
      .mv_group = ordered(integer(0))
    ))
  }
  per <- imap(comps, function(comp, nm) {
    one <- tf_unnest(comp, arg = arg, interpolate = interpolate)
    one$.component <- ordered(nm, levels = nms)
    one
  })
  long <- dplyr::bind_rows(per)
  # group-contiguous, arg-sorted within each (id, component) path
  long <- long[order(long$id, long$.component, long$arg), , drop = FALSE]
  # collision-free integer interaction code: pasted "id.component" labels can
  # merge distinct pairs (e.g. ("a.b", "c") and ("a", "b.c"))
  grp <- (match(as.character(long$id), unique(as.character(long$id))) - 1L) *
    length(nms) +
    as.integer(long$.component)
  long$.mv_group <- ordered(grp, levels = sort(unique(grp)))
  long
}

#' @export
#' @rdname tf_unnest
tf_unnest.data.frame <- function(
  data,
  cols,
  arg,
  interpolate = TRUE,
  keep_empty = FALSE,
  ptype = NULL,
  names_sep = "_",
  names_repair = "check_unique",
  ...
) {
  if (missing(cols)) {
    tf_cols <- names(data)[map_lgl(data, is_tf)]
    cols <- expr(!!!syms(tf_cols))
    cli::cli_warn(
      "{.arg cols} is now required. Please use {.code cols = {expr_text(cols)}}"
    )
  }

  ret <- tf_evaluate.data.frame(data, {{ cols }}, arg = arg) |>
    tidyr::unnest(
      cols = {{ cols }},
      keep_empty = keep_empty,
      ptype = ptype,
      names_sep = names_sep,
      names_repair = names_repair
    )
  ret
}
