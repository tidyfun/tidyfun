#' @import mgcv
smooth_spec_wrapper <- function(spec) {
  function(argvals) {
    mgcv::Predict.matrix(object = spec, data = data.frame(argvals = argvals))
  }
} 

new_fbase <- function(data, regular, basis = 'ps', domain = NULL, range = NULL, 
    penalized = TRUE, signif = 4, ...) {
  data$argvals <- .adjust_resolution(data$argvals, signif, unique = FALSE)
  argvals_u <- mgcv::uniquecombs(data$argvals, ordered = TRUE)
  s_args <- list(...)[names(list(...)) %in% names(formals(mgcv::s))]
  magic_args <- list(...)[names(list(...)) %in% names(formals(mgcv::magic))]
  if (!("sp" %in% names(magic_args))) magic_args$sp <- -1
  s_call <- as.call(c(quote(s), quote(argvals), flatten(list(bs = basis, s_args))))
  spec_object <- smooth.construct(eval(s_call), 
    data = data.frame(argvals = argvals_u$x), knots = NULL)
  n_evaluations <- table(data$id)
  underdet <- n_evaluations < spec_object$bs.dim
  eval_list <- split(data$data, data$id)
  if (!penalized) {
    if (any(underdet)) {
      warning("More basis functions than evaluations.", 
        " Interpolation will be unreliable.")
    }
    if (regular) {
      eval_matrix <- do.call(cbind, eval_list)
      coef_list <- qr.coef(qr = qr(spec_object$X), 
        y = eval_matrix)
      coef_list <- split(coef_list, col(coef_list))
      pve <- 1 - apply(qr.resid(qr = qr(spec_object$X), 
        y = eval_matrix), 2, var)/apply(eval_matrix, 2, var)
    } else {
      index_list <- split(attr(argvals_u, "index"), data$id)
      coef_list <- map2(index_list, eval_list, 
        ~ qr.coef(qr=qr(spec_object$X[.x,]), y = .y))
      pve <- unlist(map2(index_list, eval_list, 
        ~ 1 - var(qr.resid(qr=qr(spec_object$X[.x,]), y = .y))/var(.y)))
    }
    # need to remove NAs if dim(basis) > length(argvals)
    coef_list[underdet] <- map(coef_list[underdet], na_to_0) 
  } else {
    index_list <- split(attr(argvals_u, "index"), data$id)
    coef_list <- map2(index_list, eval_list, 
      ~ magic_smooth_coef(.y, .x, spec_object, magic_args))
    pve <- unlist(map(coef_list, 2))
    coef_list <- map(coef_list, 1)
  }
  message("Percentage of input data variance preserved (per functional observation, approx.):")
  print(summary(pve, digits = 3))

  domain <- domain %||% range(argvals_u)
  range <- range %||% range(data$data, na.rm = TRUE) #FIXME: should be range(X%*%coef)?
  names(coef_list) <- names(coef_list) %||% seq_along(coef_list)
  basis_constructor <- smooth_spec_wrapper(spec_object)
  structure(coef_list, 
    domain = domain,
    range = range,
    basis = memoise(basis_constructor),
    basis_label = deparse(s_call, width.cutoff = 60)[1],
    basis_matrix = spec_object$X,
    argvals = argvals_u$x,
    signif_argvals = signif, 
    class = c("fbase", "fvector"))
}

magic_smooth_coef <- function(evaluations, index, spec_object, magic_args) {
  m <- do.call(mgcv::magic, 
    c(list(y = evaluations, X = spec_object$X[index,], S = spec_object$S), 
      flatten(list(off = 1, magic_args))))
  list(coef = m$b, pve = 1 - m$scale/var(evaluations))
}

#-------------------------------------------------------------------------------

#' @export
fbase <- function(data, ...) UseMethod("fbase")

#' @export
fbase.data.frame <- function(data, id = 1, argvals = 2, value = 3, basis = 'ps', 
    domain = NULL, range = NULL, penalized = TRUE, signif = 4, ...) {
  data <- na.omit(data[, c(id, argvals, value)])
  colnames(data) <- c("id", "argvals", "data")
  stopifnot(nrow(data) > 0, 
    is.numeric(data[[2]]), 
    is.numeric(data[[3]]))
  regular <- length(unique(table(data[[1]]))) == 1
  new_fbase(data, regular, basis = basis, domain = domain, range = range, 
    penalized = penalized, signif = signif, ...)
}

#' @export
fbase.matrix <- function(data, argvals = NULL, basis = 'ps', 
  domain = NULL, range = NULL, penalized = TRUE, signif = 4, ...) {
  stopifnot(is.numeric(data))
  argvals <- unlist(find_argvals(data, argvals))
  id <- make.unique(rownames(data) %||% seq_len(dim(data)[1]))
  data <- na.omit(tibble::data_frame(id = id[row(data)], argvals = argvals[col(data)], 
    data = as.vector(data)))
  regular <- length(unique(table(data[[1]]))) == 1
  new_fbase(data, regular, basis = basis, domain = domain, range = range, 
    penalized = penalized, signif = signif, ...)
}  

#' @export
fbase.list <- function(data, argvals = NULL, basis = 'ps', 
  domain = NULL, range = NULL, penalized = TRUE, signif = 4, ...) {
  vectors <- sapply(data, is.numeric)
  stopifnot(all(vectors) | !any(vectors))
  if (all(vectors)) {
    lengths <- sapply(data, length)
    if (all(lengths == lengths[1])) {
      data <- do.call(rbind, data)
      #dispatch to matrix method
      args <- list(data, argvals, basis = basis, domain = domain, range = range, 
        penalized = penalized, signif = signif, ...)
      return(do.call(fbase, args))
    } else {
      stopifnot(!is.null(argvals), length(argvals) == length(data), 
        all(sapply(argvals, length) == lengths))
      data <- map2(argvals, data, ~as.data.frame(cbind(argvals = .x, data = .y)))
    }
  }
  dims <- map(data, dim)
  stopifnot(all(sapply(dims, length) == 2), all(map(dims, ~.x[2]) == 2),
    all(rapply(data, is.numeric)))
  data <- data_frame(id = make.unique(names(data) %||% seq_along(data)), 
      funs = data) %>% tidyr::unnest
  #dispatch to data.frame method
  fbase(data, basis = basis, domain = domain, range = range, 
    penalized = penalized, signif = signif, ...)
}
