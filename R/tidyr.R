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
#' @importFrom rlang is_empty :=
#' @importFrom tidyselect vars_select
#' @importFrom stringr str_replace
#' @export
#' @seealso dplyr::select() tfd()
tf_gather <- function(data, ..., key = ".tfd", arg = NULL, domain = NULL, 
    evaluator = approx_linear, signif = 4) {
  key_var <- quo_name(enexpr(key))
  search_key <- isTRUE(key == ".tfd")
  quos <- quos(...)
  if (rlang::is_empty(quos)) {
    gather_vars <- names(data)
  } else {
    gather_vars <- unname(tidyselect::vars_select(names(data), !!!quos))
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
        tfd(tfd_data, arg = arg, domain = domain, evaluator = !! evaluator, 
          signif = signif)) 
}
