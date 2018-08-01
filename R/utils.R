ensure_list <- function(x) if (!is.list(x)) list(x) else x

unique_id <- function(x)  {
  if (!any(duplicated(x))) return(x)
  if (is.character(x))  x <- sub("$^", "?", x)
  x <- make.unique(make.names(as.character(x)))
  #TODO: make sure this has the correct order (here or in converters?)
  x
} 


na_to_0 <- function(x) {
  x[is.na(x)] <- 0
  x
}

#' @importFrom stringr str_extract
find_arg <- function(data, arg) {
  if (is.null(arg)) {
    names <- dimnames(data)[[2]]
    suppressWarnings(arg <- as.numeric(names))
    if (is.null(arg) | any(is.na(arg))) {
      #extract number-strings
      # will interpret separating-dashes as minus-signs, so functions may run 
      # backwards.
      # regex adapted from https://www.regular-expressions.info/floatingpoint.html
      arg <- str_extract(names,
        "[-+]?(0|(0\\.[0-9]+)|([1-9][0-9]*\\.?[0-9]*))([eE][-+]?[0-9]+)?$")
      suppressWarnings(arg <- as.numeric(arg))
      if (length(unique(arg))  != dim(data)[2]) arg <- NULL
    }
    if (is.null(arg) | any(is.na(arg))) {
      message("Column names not suitable as arg. Using 1:ncol(data).")
      arg <- numeric(0)
    }
  }
  if (!length(arg)) arg <- seq_len(dim(data)[2])
  stopifnot(length(arg)  == dim(data)[2], 
    is.numeric(arg), all(!is.na(arg)))
  list(arg)
}

#' @import checkmate
assert_arg <- function(arg, x){
  if (is.list(arg)) {
    assert_true(length(arg) %in% c(1, length(x)))
    map(arg, ~ assert_arg_vector(., x = x))
  } else {
    assert_arg_vector(arg, x)
  }
}
assert_arg_vector <- function(arg, x) {
  assert_numeric(arg, any.missing = FALSE, unique = TRUE,
    lower = domain(x)[1], upper = domain(x)[2])
}



# #TODO: write proper tests for this
# check_interpolation <- function(x, arg){
#   UseMethod("check_interpolation")
# }
# check_interpolation.tfd_reg <- function(x, arg){
#   original <- arg(x)
#   if (is.list(arg)) {
#     map(arg, ~ !(. %in% original))
#   } else {
#     !(arg %in% original)
#   }
# }
# check_interpolation.tfd_irreg <- function(x, arg) {
#   original <- arg(x)
#   if (is.list(arg)) {
#     map2(arg, original, ~ !(.x %in% .y))
#   } else {
#     map(original, ~ !(arg %in% .x))
#   }
# }

adjust_resolution <- function(arg, f) {
  signif <- attr(f, "signif_arg")
  .adjust_resolution(arg, signif)
}

.adjust_resolution <- function(arg, signif, unique = TRUE){
  u <- if (unique) base::unique else function(x) x
  if (is.list(arg)) {
    map(arg, ~ u(signif(., signif)))
  } else {
    u(signif(arg, signif))
  }  
}

is_equidist <- function(f) {
  if (is_irreg(f)) return(FALSE)
  n_diffs <- map_lgl(ensure_list(arg(f)), 
    ~ round(diff(.x), attr(f, "signif")) %>% duplicated %>% tail(-1) %>% all)
  all(n_diffs)
}


compare_tf_attribs <- function(e1, e2, ignore = c("names", "id")) {
# TODO: better way to check evaluator/basis functions?
  a1 <- attributes(e1)
  a2 <- attributes(e2)
  attribs <- union(names(a1), names(a2))
  if (length(ignore)) attribs <- attribs[!(attribs %in% ignore)]
  .compare <- function(a, b) {
    if (is.null(a) != is.null(b)) return(FALSE)
    suppressWarnings(
      if (is.function(a)) {
        #FIXME: this is not reliable/useful but prob. impossible to solve
        #generally: would need to know which (functional) objects in the enclosure
        #of these functions are relevant for comparison -- comparing all is too
        #strict but comparing none is rather dangerous. Right now the function
        #bodies all look the same since they share a common wrapper.... Fingers
        #crossed relevant differences get picked up by differences in the label or
        #basis attributes...
        if (is.memoised(a)) {
          identical(environment(a)[["_f"]], environment(b)[["_f"]], 
            ignore.environment = TRUE)
        } else identical(a, b, ignore.environment = TRUE)
      } else {
        if (is.list(a)) {
          all(unlist(map2(a, ensure_list(b), .compare)))
        } else isTRUE(all.equal(a, b))
      })
  }  
  ret <- map(attribs, ~.compare(a1[[.]], a2[[.]]))
  names(ret) <- attribs
  unlist(ret)
}

#-------------------------------------------------------------------------------

#from refund
#' @importFrom stats complete.cases
irreg2mat <- function (ydata, binning = FALSE, maxbins = 1000) 
{
  ydata <- ydata[complete.cases(ydata), ]
  nobs <- length(unique(ydata$.id))
  newid <- as.numeric(as.factor(ydata$.id))
  bins <- sort(unique(ydata$.index))
  if (binning && (length(bins) > maxbins)) {
    binvalues <- seq((1 - 0.001 * sign(bins[1])) * bins[1], 
      (1 + 0.001 * sign(bins[length(bins)])) * bins[length(bins)], 
      l = maxbins + 1)
    bins <- binvalues
    binvalues <- head(filter(binvalues, c(0.5, 0.5)), -1)
  }
  else {
    binvalues <- bins
    bins <- c((1 - 0.001 * sign(bins[1])) * bins[1], bins[-length(bins)], 
      (1 + 0.001 * sign(bins[length(bins)])) * bins[length(bins)])
    if (bins[1] == 0) 
      bins[1] <- -0.001
    if (bins[length(bins)] == 0) 
      bins[length(bins)] <- 0.001
  }
  newindex <- cut(ydata$.index, breaks = bins, include.lowest = TRUE)
  Y <- matrix(NA, nrow = nobs, ncol = nlevels(newindex))
  colnames(Y) <- binvalues
  attr(Y, "index") <- binvalues
  Y[cbind(newid, as.numeric(newindex))] <- ydata$.value
  return(Y)
}
