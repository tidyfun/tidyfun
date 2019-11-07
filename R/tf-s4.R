#' @include tfd-class.R tfb-class.R tfb-spline.R
#' @importFrom methods setClass setOldClass
setClass("tf",
         contains = "structure",
         slots = c(
           domain = "numeric",
           arg = "vector",
           resolution = "numeric"
         ),
         prototype = structure(
           list(),
           domain = c(NA_real_, NA_real_),
           arg = numeric(),
           resolution = NA_real_
         )
)
setOldClass("tf", S4Class = "tf")

setClass("tfd",
         contains = "tf",
         slots = c(
           evaluator = "function",
           evaluator_name = "character"
         ),
         prototype = structure(
           list(),
           domain = c(NA_real_, NA_real_),
           arg = numeric(),
           resolution = NA_real_,
           evaluator = function(x, arg, evaluations) NULL,
           evaluator_name = NA_character_
         )
)
setOldClass("tfd", S4Class = "tfd")

setClass("tfd_reg", 
         contains = "tfd",
         slots = c(
           arg = "list"
         ),
         prototype = structure(
           list(),
           domain = c(NA_real_, NA_real_),
           arg = list(),
           resolution = NA_real_,
           evaluator = function(x, arg, evaluations) NULL,
           evaluator_name = NA_character_
         )
)
setOldClass("tfd_reg", S4Class = "tfd_reg")

setClass("tfd_irreg", contains = "tfd")
setOldClass("tfd_irreg", S4Class = "tfd_irreg")
         
setClass("tfb",
         contains = "tf",
         slots = c(
           arg = "numeric",
           basis = "function",
           basis_label = "character",
           basis_args = "list",
           basis_matrix = "matrix",
           family = "function"
         ),
         prototype = structure(
           list(),
           domain = c(NA_real_, NA_real_),
           arg = numeric(),
           resolution = NA_real_,
           basis = function(x, arg, evaluations) NULL,
           basis_label = NA_character_,
           basis_args = list(),
           basis_matrix = matrix(NA_real_, 1, 1),
           family = function() NULL
         )
)
setOldClass("tfb", S4Class = "tfb")

