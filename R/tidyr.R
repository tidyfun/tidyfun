#' Gather all columns representing functional measurements into a `tf`-object
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
#' @param key the name of the created `tfd`-column. Defaults to `".tfd"`, and
#'   the function will try to guess the name based on the column names of the
#'   gathered columns in this case. If a common prefix of all column names 
#'   is found, this is used instead. You also get a message about this.
#' @param arg If not provided, will be guessed from the column names as well. 
#'   See [tfd()].
#' @inheritParams tfd 
#' @return a modified `data.frame` with a `tfd` column replacing the `...`.
#' @importFrom rlang is_empty :=  quo_name enexpr
#' @importFrom tidyselect vars_select
#' @importFrom stringr str_replace
#' @export
#' @seealso dplyr::select() tfd() tf_nest() tf_unnest()
#' @examples
#' (d <- as.tbl(data.frame(refund::DTI[1:5,]$cca[, 1:10])))
#' tf_gather(d)
#' tf_gather(d, key = "cca_tf")
#' tf_gather(d, arg = seq(0, 1, l = 10))$cca
#' (d2 <- bind_cols(id = rownames(d), d))
#' tf_gather(d2, -id) # tf_gather(d2, matches("cca")); tf_gather(d2, -1); etc
tf_gather <- function(data, ..., key = ".tfd", arg = NULL, domain = NULL, 
    evaluator = approx_linear, signif = 4) {
  key_var <- quo_name(enexpr(key))
  evaluator <- quo_name(enexpr(evaluator))
  search_key <- isTRUE(key == ".tfd")
  quos <- quos(...)
  if (rlang::is_empty(quos)) {
    gather_vars <- names(data)
  } else {
    gather_vars <- unname(vars_select(names(data), !!!quos))
  }
  if (rlang::is_empty(gather_vars)) {
    return(data)
  }
  #turn matrix column into regular columns:
  if (length(gather_vars) == 1) {
    if (is.matrix(data[[gather_vars]]) & search_key) {
      key_var <- gather_vars
      search_key <- FALSE
      message("creating new tfd-column <", key_var, ">")
    }  
  }
  other_vars <- setdiff(names(data), gather_vars)
  
  tfd_data <- data %>% select(gather_vars) %>% as.matrix
  if (search_key) {
    #see also find_arg: will interpret separating-dashes as minus-signs
    # regex adapted from https://www.regular-expressions.info/floatingpoint.html
    found_key <- unique(str_replace(colnames(tfd_data),
      "[-+]?(0|(0\\.[0-9]+)|([1-9][0-9]*\\.?[0-9]*))([eE][-+]?[0-9]+)?$", ""))
    # assume trailing punctuation is separator:
    found_key <- str_replace(found_key, "[:punct:]$", "")
    if (length(found_key) == 1 & all(found_key != "")) {
      key_var <- found_key
      message("creating new tfd-column <", key_var, ">")
    } 
  }

  data %>% select(-gather_vars) %>% 
    mutate(!!key_var := 
        tfd(tfd_data, arg = arg, domain = domain, evaluator = !!evaluator, 
          signif = signif)) 
}

# ------------------------------------------------------------------------------

#' Turn "long" tables into tidy data frames with `tf`-objects
#'
#' Similar in spirit to [tidyr::nest()]. This turns tables in "long" format,
#' where one column (`.id`) defines the unit of observation, one column (`.arg`)
#' defines the evaluation of the functional observations, and other columns (`...`)
#' define the values of the functions into a (much shorter) table containing
#' `tfd`-objects. All other variables are checked for constancy over `.id` and
#' appended as well.
#'
#' @param data a data frame
#' @param ... A selection of columns. If empty, all variables except the 
#'   `.id` and `.arg` columns are selected. You can supply bare variable names, 
#'   select all variables between `x` and `z` with `x:z`, exclude `y` with `-y`. 
#'   For more options, see the [dplyr::select()] documentation. 
#'   See also the section on selection rules below.
#' @param .id the (bare or quoted) name of the column defining the different observations
#' @param .arg the (bare or quoted) name of the column defining the `arg`-values of the observed functions
#' @return a data frame with (at least) `.id` and `tfd` columns
#' @export
#' @seealso tf_gather() tf_unnest()
tf_nest <- function(data, ..., .id = "id", .arg = "arg") {
  stopifnot(!missing(data)) 
  id_var <- quo_name(enexpr(.id))
  arg_var <- quo_name(enexpr(.arg))
  quos <- quos(...)
  if (is_empty(quos)) {
    value_vars <- setdiff(names(data), c(id_var, arg_var))
  } else {
    value_vars <- unname(vars_select(names(data), !!!quos))
  }
  if (is_empty(value_vars)) {
    return(data)
  }
  remaining <- setdiff(names(data), c(id_var, arg_var, value_vars))
  # check that nesting is possible without information loss
  ret <- select(data, !!id_var, !!remaining) %>%
    group_by(!!as.name(id_var))
  not_constant <- ret %>% 
    summarise_all(n_distinct) %>% select(-!!id_var) %>% 
    summarise_all(function(x) !all(x == 1L)) %>% 
    select_if(isTRUE)
  if (ncol(not_constant)) {
    stop("Columns ", paste(names(not_constant), collapse = ", "), 
      " are not constant for all levels of the id-variable.")
  }
  ret <- slice(ret, 1) %>% ungroup
  tfd_list <- map(value_vars, 
    ~ select(data, id_var, arg_var, .x) %>%  tfd)
  names(tfd_list) <- value_vars
  for (v in value_vars) {
    ret[[v]] <- tfd_list[[v]]
  }
  ret
} 

#-------------------------------------------------------------------------------

#' Turn data frames with `tf`-objects / list columns into "long" tables.
#'
#' Similar in spirit to [tidyr::unnest()], the reverse of `tf_nest`.
#' Note 
#'
#' @param data a data frame
#' @param .arg optional values for the `arg` argument of [evaluate.data.frame()]
#' @inheritParams evaluate.data.frame
#' @inheritParams tidyr::unnest
#' @return a "long" data frame with 
#' @export
#' @seealso tf_gather() tf_unnest() evaluate.data.frame
tf_unnest <- function(data, ..., .arg, .drop = NA, .id = "id", .sep = "_", 
    .preserve = NULL) {
  preserve <- unname(vars_select(names(data), !!!enquo(.preserve)))
  tfds <- unname(vars_select(names(data), !!!quos(...)))
  ret <- evaluate.data.frame(data, arg = .arg, !!!tfds)
  # don't unnest unevaluated tf-columns:
  preserve <- unique(c(preserve, names(ret)[map_lgl(ret, is_tf)]))
  ret <- unnest(ret, .drop = .drop, .id = .id, .sep = .sep, .preserve = preserve)
  arg_merge <- id_merge <- FALSE
  # drop duplicated arg columns if possible: 
  arg_pattern <- paste0(.sep, "arg$")
  same_arg <- select(ret, matches(!!!arg_pattern)) %>% as.matrix %>% t %>% 
    duplicated %>% tail(-1) %>% all
  if (same_arg) {
    arg_merge  <- length(vars_select(names(ret), matches(!!!arg_pattern))) > 1 &
      !("arg" %in% names(ret))
    if (arg_merge) {
      new_arg <- select(ret, matches(!!!arg_pattern)) %>% names %>% head(1)
      ret <- rename(ret, arg = !!new_arg) %>% select(-matches(!!!arg_pattern))
    }  
  }
  # drop duplicated id columns if possible: 
  id_pattern <- paste0(.sep, "id$")
  same_id <- select(ret, matches(!!!id_pattern)) %>% as.matrix %>% t %>% 
    duplicated %>% tail(-1) %>% all
  if (same_id) {
    id_merge  <- length(vars_select(names(ret), matches(!!!id_pattern))) > 1 &
      !("id" %in% names(ret))
    if (id_merge) {
      new_id <- select(ret, matches(!!!id_pattern)) %>% names %>% head(1)
      ret <- rename(ret, id = !!new_id) %>% select(-matches(!!!id_pattern))
    }   
  }
  if (id_merge | arg_merge) {
    message("Removing duplicate columns of ", 
      ifelse(id_merge, "ids", ""),
      ifelse(id_merge & arg_merge, ", ", ""),
      ifelse(arg_merge, "arg-values", ""))
  }  
  ret
} 

