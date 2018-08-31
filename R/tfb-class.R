# input homogenizers
df_2_df <- function(data, id = 1, arg = 2, value = 3) {
  data <- na.omit(data[, c(id, arg, value)])
  colnames(data) <- c("id", "arg", "data")
  stopifnot(nrow(data) > 0, 
    is.numeric(data[[2]]), 
    is.numeric(data[[3]]))
  data 
}

mat_2_df <- function(x, arg) {
  stopifnot(is.numeric(x))
  id <- unique_id(rownames(x)) %||% seq_len(dim(x)[1])
  df_2_df(data_frame(id = id[row(x)], arg = arg[col(x)], 
    data = as.vector(x)))
}
  

#' @import mgcv
smooth_spec_wrapper <- function(spec, deriv = 0, eps = 1e-6) {
  stopifnot(deriv %in% c(-1, 0, 1, 2), isTRUE(eps > 0))
  if (deriv == 0) {
    return(function(arg) {
      mgcv::Predict.matrix(object = spec, data = data.frame(arg = arg))
    })  
  } 
  if (deriv == 1) {
    return(function(arg) {
        X <- mgcv::Predict.matrix(object = spec, 
          data = data.frame(arg = c(arg + eps, arg - eps)))
        (X[1:length(arg), ] - X[-(1:length(arg)), ]) / (2 * eps)
      })
  }
  if (deriv == 2) {
    return(function(arg) {
      g <- length(arg)
      X <- mgcv::Predict.matrix(object = spec, 
        data = data.frame(arg = c(arg + eps, arg, arg - eps)))
      (X[1:g, ] - (2 * X[(g + 1):(2 * g),]) + X[-(1:(2 * g)), ]) / eps^2
    })
  }
  if (deriv == -1) {
    return(function(arg) {
      #make sure quadrature runs over entire range up to the new arg
      # --> have to re-use original grid
      arg_orig <- spec$Xu[spec$Xu <= max(arg)]
      arg_interleave <- sort(unique(c(arg_orig, arg)))
      new_args <- which(arg_interleave %in% arg)
      X <- mgcv::Predict.matrix(object = spec, 
        data = data.frame(arg = arg_interleave))
      apply(X, 2, function(arg, x) cumsum(quad_trapez(arg, x)), 
        arg = arg_interleave)[new_args, ]
    })
  }
} 

#' @importFrom stats var na.omit median
mgcv_tfb <- function(data, regular, domain = NULL,   
    penalized = TRUE, resolution = NULL, verbose = TRUE, ...) {
  domain <- domain %||% range(data$arg)
  arg_u <- mgcv::uniquecombs(data$arg, ordered = TRUE)
  resolution <- resolution %||%  get_resolution(arg_u)
  s_args <- list(...)[names(list(...)) %in% names(formals(mgcv::s))]
  if (!("bs" %in% names(s_args))) s_args$bs <- "cr"
  if (!("k" %in% names(s_args))) s_args$k <- min(25, nrow(arg_u))
  magic_args <- list(...)[names(list(...)) %in% names(formals(mgcv::magic))]
  if (!("sp" %in% names(magic_args))) magic_args$sp <- -1
  s_call <- as.call(c(quote(s), quote(arg), s_args))
  s_spec <- eval(s_call)
  #if ("deriv" %in% names(list(...))) s_spec$deriv <- list(...)$deriv
  #if ("mono" %in% names(list(...))) s_spec$mono <- list(...)$mono
  spec_object <- smooth.construct(s_spec, 
    data = data.frame(arg = arg_u$x), knots = NULL)
  n_evaluations <- table(data$id)
  underdet <- n_evaluations < spec_object$bs.dim
  # explicit factor-conversion to avoid reordering
  data$id <- factor(data$id, levels = unique(as.character(data$id)))
  eval_list <- split(data$data, data$id)
  if (!penalized) {
    if (any(underdet) & verbose) {
      warning("More basis functions than evaluations.", 
        " Interpolation will be unreliable.")
    }
    if (regular) {
      eval_matrix <- do.call(cbind, eval_list)
      qr_basis <- qr(spec_object$X)
      coef_list <- qr.coef(qr = qr_basis, y = eval_matrix)
      coef_list <- split(coef_list, col(coef_list))
      pve <- 1 - apply(qr.resid(qr = qr_basis, 
        y = eval_matrix), 2, var)/apply(eval_matrix, 2, var)
    } else {
      index_list <- split(attr(arg_u, "index"), data$id)
      coef_list <- map2(index_list, eval_list, 
        ~ qr.coef(qr=qr(spec_object$X[.x,]), y = .y))
      pve <- unlist(map2(index_list, eval_list, 
        ~ 1 - var(qr.resid(qr=qr(spec_object$X[.x,]), y = .y))/var(.y)))
    }
    # need to remove NAs if dim(basis) > length(arg)
    coef_list[underdet] <- map(coef_list[underdet], na_to_0) 
  } else {
    index_list <- split(attr(arg_u, "index"), data$id)
    coef_list <- map2(index_list, eval_list, 
      ~ magic_smooth_coef(.y, .x, spec_object, magic_args))
    pve <- unlist(map(coef_list, 2))
    coef_list <- map(coef_list, 1)
  }
  if (verbose) {
    message("Percentage of raw input data variance preserved in basis representation:\n", 
      "(per functional observation, tf_approx.):")
    print(summary(round(100 * pve, 1)))
  }
  names(coef_list) <- levels(data$id)
  basis_constructor <- smooth_spec_wrapper(spec_object)
  ret <- structure(coef_list, 
    domain = domain,
    basis = memoise(basis_constructor),
    basis_label = deparse(s_call, width.cutoff = 60)[1],
    basis_args = s_args,
    basis_matrix = spec_object$X,
    arg = arg_u$x,
    resolution = resolution, 
    class = c("tfb", "tf"))
  ret
}

magic_smooth_coef <- function(evaluations, index, spec_object, magic_args) {
  m <- do.call(mgcv::magic, 
    c(list(y = evaluations, X = spec_object$X[index,], S = spec_object$S), 
      flatten(list(off = 1, magic_args))))
  list(coef = m$b, pve = 1 - m$scale/var(evaluations))
}

#-------------------------------------------------------------------------------
#' Constructors & convertors for functional data in (spline) basis representation
#' 
#' Various constructor and conversion methods.
#'
#' `tfb` tries to represent the input data as linear
#' combinations of a set of common spline basis functions identical for all
#' observations and coefficient vectors estimated for each observation. The
#' basis used is set up via a call to [mgcv::s()] and all the spline bases
#' discussed in [mgcv::smooth.terms] are available, in principle. Depending on
#' the value of the `penalized`-flag, the coefficient vectors for each
#' observation are then estimated via fitting a (small) GAM for each observation
#' via [mgcv::magic()] or via simple ordinary least squares.
#'
#' After the "smoothed" representation is computed, the amount of smoothing that
#' was performed is reported in terms of the "percentage of variance preserved",
#' which is the variance of the smoothed function values divided by the variance
#' of the original values. The `...` arguments supplies arguments to both the
#' spline basis set up (via [mgcv::s()]) and the estimation (via
#' [mgcv::magic()]), most important: how many basis functions `k` the spline
#' basis should have, the default is 25. 
#' 
#' See [tfb_fpc()] for using an FPC representation with an orthogonal basis estimated from the
#' data instead.
#' 
#' @param data a `matrix`, `data.frame` or `list` of suitable shape, or another
#'   `tf`-object.
#' @return an `tfb`-object (or a `data.frame`/`matrix` for the conversion
#'   functions, obviously.)
#' @rdname tfb
#' @seealso [tfb_fpc()]
#' @export
tfb <- function(data, ...) UseMethod("tfb")


#' @export
#' @inheritParams tfd.data.frame
#' @param penalized should the coefficients of the basis representation be estimated
#'   via [mgcv::magic()] (default) or ordinary least squares.
#' @param ... **for `tfb`**: arguments to the calls to [mgcv::s()] setting up the basis and
#'   [mgcv::magic()] (if `penalized` is TRUE). If not user-specified here,
#'   `tidyfun` uses `k=15` cubic regression spline basis functions (i.e., `bs =
#'   "cr"`) by default, but at least how many basis functions `k` the spline
#'   basis should have probably needs to be set manually.\cr
#'   **for `as.tfb`:** use this to give arguments to [tfb()] or [tfb_fpc()].
#' @rdname tfb
#' @export
tfb.data.frame <- function(data, id = 1, arg = 2, value = 3,  
    domain = NULL, penalized = TRUE, resolution = NULL, ...) {
  data <- df_2_df(data, id, arg, value)
  regular <- n_distinct(table(data[[1]])) == 1
  ret <- mgcv_tfb(data, regular, domain = domain,   
    penalized = penalized, resolution = resolution, ...)
  assert_arg(arg(ret), ret)
  ret
}

#' @rdname tfb
#' @export
tfb.matrix <- function(data, arg = NULL, 
  domain = NULL,   penalized = TRUE, resolution = NULL, ...) {
  arg <- unlist(find_arg(data, arg))
  data_names <- rownames(data)
  data <- mat_2_df(data, arg)
  regular <- n_distinct(table(data[[1]])) == 1
  ret <- mgcv_tfb(data, regular, domain = domain,   
    penalized = penalized, resolution = resolution, ...)
  names(ret) <- data_names
  assert_arg(arg(ret), ret)
  ret
}
#' @rdname tfb
#' @export
tfb.numeric <- function(data, arg = NULL, 
  domain = NULL, penalized = TRUE, resolution = NULL, ...) {
  data <- t(as.matrix(data))
  tfb(data = data, arg = arg, domain = domain, penalized = penalized,
    resolution = resolution, ...)
}

#' @rdname tfb
#' @export
tfb.list <- function(data, arg = NULL,  
  domain = NULL,   penalized = TRUE, resolution = NULL, ...) {
  vectors <- sapply(data, is.numeric)
  stopifnot(all(vectors) | !any(vectors))
  names_data <- names(data)
  if (all(vectors)) {
    lengths <- sapply(data, length)
    if (all(lengths == lengths[1])) {
      data <- do.call(rbind, data)
      #dispatch to matrix method
      args <- list(data, arg,  domain = domain,   
        penalized = penalized, resolution = resolution, ...)
      return(do.call(tfb, args))
    } else {
      stopifnot(!is.null(arg), length(arg) == length(data), 
        all(sapply(arg, length) == lengths))
      data <- map2(arg, data, ~as.data.frame(cbind(arg = .x, data = .y)))
    }
  }
  dims <- map(data, dim)
  stopifnot(all(sapply(dims, length) == 2), all(map(dims, ~.x[2]) == 2),
    all(rapply(data, is.numeric)))
  data <- data_frame(id = unique_id(names(data)) %||% seq_along(data), 
      funs = data) %>% tidyr::unnest
  #dispatch to data.frame method
  ret <- tfb(data, domain = domain,   
    penalized = penalized, resolution = resolution, ...)
  names(ret) <- names_data
  ret
}

#' @rdname tfb
#' @export
tfb.tfd <- function(data, arg = NULL, 
  domain = NULL, penalized = TRUE, resolution = NULL, ...) {
  arg <- arg %||% arg(data)
  domain <- domain %||% tf_domain(data)
  resolution <- resolution %||% resolution(data)
  names_data <- names(data)
  data <- as.data.frame(data, arg)
  ret <- tfb(data, domain = domain,   
    penalized = penalized, resolution = resolution, ...)
  names(ret) <- names_data
  ret
}

#' @rdname tfb
#' @export
tfb.tfb <- function(data, arg = NULL,
  domain = NULL, penalized = TRUE, resolution = NULL, ...) {
  arg <- arg %||% arg(data)
  resolution <- resolution %||% resolution(data)
  domain <- domain %||% tf_domain(data)
  s_args <- modifyList(attr(data, "basis_args"),
    list(...)[names(list(...)) %in% names(formals(mgcv::s))])
  names_data <- names(data)
  data <- as.data.frame(data, arg = arg)
  ret <- do.call("tfb", c(list(data), domain = domain,
    penalized = penalized, resolution = resolution, s_args))
  names(ret) <- names_data
  ret
}
