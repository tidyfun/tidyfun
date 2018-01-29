#' Unnest a `data.frame` with `fvector`-columns
#' 
#' This is a wrapper around [tidyr::unnest()] that extends the functionality
#' provided for list-columns to `fvector`-columns, i.e., it creates new "long" 
#' `data.frame` containing the function evaluations
#' 
#' @inheritParams tidyr::unnest
#' @param argvals evaluation grid for fvector columns. Can be a named list giving one j-vector for 
#'   each `fvector` to unlist. Used as the argument for the `as.data.frame`-method for the respective 
#'   `fvector`-columns. NOT IMPLEMENTED YET
#' @import tidyr rlang
#' @export
#' @md
unnest.data.frame <- function(data, ..., argvals = NULL, .drop = NA, .id = NULL,
  .sep = NULL, .preserve = NULL) {
  
  call <- match.call()
  preserve <- tidyselect::vars_select(names(data), !!! enquo(.preserve))
  quos <- quos(...)
  
  # figure out which fvector columns to unnest
  fvector_cols <- names(data)[map_lgl(data, is_fvector)]
  if (is_empty(quos)) {
    fvector_cols <- setdiff(fvector_cols, preserve)
  } else {
    fvector_cols <- intersect(fvector_cols, map_chr(quos, quo_text))
  }

  # convert them to lists of data.frames
  data <- mutate_if(data, names(data) %in% fvector_cols, 
    function(f) {
      nest(as.data.frame(f), -id)$data
    })
  
  # send pre-unnested data on to tidyr::unnest
  tidyr:::unnest.data.frame(data = data, ..., .drop = .drop, .id = .id,
    .sep = .sep, .preserve = .preserve)
}
