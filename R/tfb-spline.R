#' @importFrom stats var na.omit median gaussian
new_tfb_spline <- function(data, domain = numeric(), arg = numeric(), 
                           resolution = numeric(),
                           family = character(), 
                           penalized = TRUE, global = FALSE,
                           verbose = TRUE, ...) {
  
  if (vctrs::vec_size(data) == 0) {
    
    ret = vctrs::new_vctr(
      data,
      domain = domain,
      arg = arg, 
      resolution = resolution,
      family = family, 
      class = c("tfb_spline", "tfb", "tf"))  
    return(ret)
    
  }
  
  domain <- domain %||% range(data$arg)
  arg_u <- mgcv::uniquecombs(data$arg, ordered = TRUE)
  resolution <- resolution %||% get_resolution(arg_u)
  domain <- c(round_resolution(domain[1], resolution, -1),
              round_resolution(domain[2], resolution, 1))
  # explicit factor-conversion to avoid reordering:
  data$id <- factor(data$id, levels = unique(as.character(data$id)))
  
  s_args <- list(...)[names(list(...)) %in% names(formals(mgcv::s))]
  if (!("bs" %in% names(s_args))) s_args$bs <- "cr"
  if (s_args$bs == "ad") {
    warning("adaptive smooths with (bs='ad') not implemented yet.",
    "Changing to bs='cr'.")
    s_args$bs <- "cr"
  }
  if (!("k" %in% names(s_args))) s_args$k <- min(25, nrow(arg_u))
  gam_args <- list(...)[names(list(...)) %in% 
                          c(names(formals(mgcv::gam)), 
                            names(formals(mgcv::bam)))]
  if (!("sp" %in% names(gam_args))) gam_args$sp <- -1
  
  n_evaluations <- table(data$id)
  arg_list <- split(data$arg, data$id)
  regular <- all(duplicated(arg_list)[-1])

  s_call <- as.call(c(quote(s), quote(arg), s_args))
  s_spec <- eval(s_call)
  spec_object <- smooth.construct(s_spec, data = data.frame(arg = arg_u$x), 
                                  knots = NULL)
  spec_object$call <- s_call
  
  if (is.null(gam_args$family)) {
    gam_args$family <- gaussian()
  } 
  if (is.character(gam_args$family)) {
    gam_args$family <- get(gam_args$family, mode = "function", 
                           envir = parent.frame())
  }
  if (is.function(gam_args$family)) {
    gam_args$family <- gam_args$family()
  }

  ls_fit <- gam_args$family$family == "gaussian" & 
    gam_args$family$link == "identity"
  
  if (!penalized) {
    underdetermined <- n_evaluations <= spec_object$bs.dim
    if (any(underdetermined)) {
      stop("At least as many basis functions as evaluations for ",
           sum(underdetermined), " functions.",
           " Use penalized = TRUE or reduce k for spline interpolation.")
    }
    fit <- 
      fit_unpenalized(data = data, spec_object = spec_object, arg_u = arg_u, 
                      gam_args = gam_args, regular = regular, ls_fit = ls_fit)
  } else {
    fit <- 
      fit_penalized(data = data, spec_object = spec_object, arg_u = arg_u,
                    gam_args = gam_args, regular = regular, global = global,
                    ls_fit = ls_fit)
    if (global & verbose) {
      message(
        sprintf(
          c("Using global smoothing parameter sp = %.3g,",
            " estimated on subsample of curves."),
          fit$sp[1]
          ))
    }
  }
  if (!regular) {
    arg_u <- data.frame(x = unique(round_resolution(arg_u$x, resolution)))
    spec_object$X <- PredictMat(spec_object, data = data.frame(arg = arg_u$x))
  }
  if (verbose) {
    message(
      "Percentage of input data variability preserved in basis representation\n(",
      if (!ls_fit) "on inverse link-scale, " else NULL,
      "per functional observation, approximate):"
    )
    print(summary(round(100 * fit$pve, 1)))
  }
  
  basis_constructor <- smooth_spec_wrapper(spec_object)
  ret <- vctrs::new_vctr(fit[["coef"]],
                   domain = domain,
                   basis = memoise(basis_constructor),
                   basis_label = deparse(s_call, width.cutoff = 60)[1],
                   basis_args = s_args,
                   basis_matrix = spec_object$X,
                   arg = arg_u$x,
                   resolution = resolution,
                   family = eval(gam_args$family),
                   class = c("tfb_spline", "tfb", "tf")
  )
  ret
}


#-------------------------------------------------------------------------------

#' Spline-based representation of functional data
#' 
#' Represent curves as a weighted sum of spline basis functions.
#' 
#' The basis to be used is set up via a call to [mgcv::s()] and all the spline bases
#' discussed in [mgcv::smooth.terms()] are available, in principle. Depending on
#' the value of the `penalized`- and `global`-flags, the coefficient vectors for each
#' observation are then estimated via fitting a GAM 
#' (separately for each observation, if `!global`)
#' via [mgcv::magic()] (least square error, the default) 
#' or [mgcv::gam()] (if a `family` argument was supplied) or 
#' unpenalized least squares / maximum likelihood. 
#'
#' After the "smoothed" representation is computed, the amount of smoothing that
#' was performed is reported in terms of the "percentage of variability preserved",
#' which is the variance (explained deviance, in the general case) of the smoothed function values divided by the variance
#' of the original values (null deviance, in the general case). 
#' Reporting can be switched off with `verbose = FALSE`.
#' 
#' The `...` arguments supplies arguments to both the
#' spline basis (via [mgcv::s()]) and the estimation (via
#' [mgcv::magic()] or [mgcv::gam()]), most important:  
#' 
#' - how many basis functions `k` the spline basis should have, the default is
#' 25.
#' - which type of spline basis `bs` should be used, the default is cubic
#' regression splines (`"cr"`) - a `family`-argument to the fitters for data for
#' which squared errors are not a reasonable criterion for the representation
#' accuracy (see [mgcv::family.mgcv()] for what's available).
#' - an `sp`-argument for manually fixing the amount of smoothing (see
#' [mgcv::s()]), which (drastically) reduces the computation time. 
#' 
#' If **`global == TRUE`**, the routine first takes a subset of curves (10\% of
#' curves sampled deterministically, at most 100, at least 5) on which smoothing
#' parameters per curve are estimated and then uses the mean of the log
#' smoothing parameter of those for all curves. This can be much faster than
#' optimizing the smoothing parameter for each curve on large datasets. For very
#' sparse data, it would be preferable to estimate a joint smoothing parameter
#' directly for all curves, this is *not* what's implemented here.
#' 
#' @param global Defaults to `FALSE`. 
#' If `TRUE` and `penalized = TRUE`, all functions share the same smoothing
#'   parameter (see Details).
#' @param ...  arguments to the calls to [mgcv::s()] setting up the basis and
#'   [mgcv::magic()] or [mgcv::gam.fit()] (if `penalized` is TRUE). If not user-specified here,
#'   `tidyfun` uses `k = 25` cubic regression spline basis functions 
#'   (i.e., `bs = "cr"`) by default, but this should (!) be set appropriately.
#' @return a `tfb`-object
#' @seealso [mgcv::smooth.terms()] for spline basis options. 
tfb_spline <- function(data, ...) UseMethod("tfb_spline")

#' @export
#' @param penalized should the coefficients of the basis representation be estimated
#'   via [mgcv::magic()] (default) or ordinary least squares.
#' @describeIn tfb_spline convert data frames
tfb_spline.data.frame <- function(data, id = 1, arg = 2, value = 3,
                                  domain = NULL, penalized = TRUE, 
                                  global = FALSE, resolution = NULL, ...) {
  data <- df_2_df(data, id, arg, value)
  ret <- new_tfb_spline(data, domain = domain, penalized = penalized,
                        global = global, resolution = resolution, ...)
  assert_arg(tf_arg(ret), ret)
  ret
}


#' @export
#' @describeIn tfb_spline convert matrices
tfb_spline.matrix <- function(data, arg = NULL,
                              domain = NULL, penalized = TRUE, 
                              global = FALSE, resolution = NULL, ...) {
  arg <- unlist(find_arg(data, arg))
  
  # ensure "unique" names (principles.tidyverse.org/names-attribute.html)
  names_data <- rownames(data) # %||% rep(".", nrow(data))
  
  data <- mat_2_df(data, arg)
  ret <- new_tfb_spline(data, domain = domain, penalized = penalized,
                        global = global, resolution = resolution, ...)
  names(ret) <- names_data
  assert_arg(tf_arg(ret), ret)
  ret
}

#' @export
#' @describeIn tfb_spline convert matrices
tfb_spline.numeric <- function(data, arg = NULL,
                               domain = NULL, penalized = TRUE, 
                               global = FALSE, resolution = NULL, ...) {
  data <- t(as.matrix(data))
  tfb_spline(data = data, arg = arg, domain = domain, penalized = penalized,
      global = global, resolution = resolution, ...)
}


#' @export
#' @describeIn tfb_spline convert lists
tfb_spline.list <- function(data, arg = NULL, 
                            domain = NULL, penalized = TRUE, 
                            global = FALSE, resolution = NULL, ...) {
  vectors <- vapply(data, is.numeric, logical(1))
  stopifnot(all(vectors) | !any(vectors))
  
  # ensure "unique" names (principles.tidyverse.org/names-attribute.html)
  names_data <- names(data) # %||% rep(".", length(data))
  
  if (all(vectors)) {
    lengths <- vapply(data, length, numeric(1))
    if (all(lengths == lengths[1])) {
      data <- do.call(rbind, data)
      # dispatch to matrix method
      return(tfb_spline(data, arg, domain = domain, penalized = penalized, 
                 global = global, resolution = resolution, ...))
    } 
    stopifnot(
      !is.null(arg), length(arg) == length(data),
      all(vapply(arg, length, numeric(1)) == lengths)
    )
    data <- map2(arg, data, ~as.data.frame(cbind(arg = .x, data = .y)))
  }
  dims <- map(data, dim)
  stopifnot(
    all(vapply(dims, length, numeric(1)) == 2), all(map(dims, ~.x[2]) == 2),
    all(rapply(data, is.numeric))
  )
  data <- dplyr::tibble(
    id = unique_id(names(data)) %||% seq_along(data),
    funs = data
  ) %>% { tidyr::unnest_legacy(.) }
  # dispatch to data.frame method
  ret <- tfb_spline(data, domain = domain, penalized = penalized, 
             global = global, resolution = resolution, ...)
  names(ret) <- names_data
  ret
}


#' @export
#' @describeIn tfb_spline convert `tfd` (raw functional data)
tfb_spline.tfd <- function(data, arg = NULL,
                           domain = NULL, penalized = TRUE, 
                           global = FALSE, resolution = NULL, ...) {
  arg <- arg %||% tf_arg(data)
  domain <- domain %||% tf_domain(data)
  resolution <- resolution %||% tf_resolution(data)
  
  # ensure "unique" names (principles.tidyverse.org/names-attribute.html)
  names_data <- names(data) # %||% rep(".", length(data))
  
  data <- as.data.frame(data, arg)
  ret <- tfb_spline(data, domain = domain, penalized = penalized, 
             global = global,  resolution = resolution, ...)
  names(ret) <- names_data
  ret
}

#' @export
#' @describeIn tfb_spline convert `tfb`: modify basis representation, smoothing.
tfb_spline.tfb <- function(data, arg = NULL,
                           domain = NULL, penalized = TRUE, 
                           global = FALSE, resolution = NULL, ...) {
  arg <- arg %||% tf_arg(data)
  resolution <- resolution %||% tf_resolution(data)
  domain <- domain %||% tf_domain(data)
  s_args <- modifyList(
    attr(data, "basis_args"),
    list(...)[names(list(...)) %in% names(formals(mgcv::s))]
  )
  names_data <- names(data)
  if(vctrs::vec_size(data) == 0){
    #data = rep(0, )
    # maybe try to make an empty vector that won't break anything like matrix algebra?
    
    ret <- new_tfb_spline(data, arg = arg, domain = domain, penalized = penalized,
                          global = global, resolution = resolution, s_args)
  }else{
    data <- as.data.frame(data, arg = arg)
    ret <- do.call("tfb_spline", c(list(data),
                                   domain = domain, global = global,
                                   penalized = penalized, resolution = resolution, s_args
    ))
  }
  
  names(ret) <- names_data
  ret
}

#' @export
#' @describeIn tfb_spline convert `tfb`: default method, returning prototype when data is NULL
tfb_spline.default = function(data, arg = NULL,
                              domain = NULL, penalized = TRUE, 
                              global = FALSE, resolution = NULL, ...) {
  
  if (!missing(data)) {
    message("input `data` not recognized class; returning prototype of length 0")
  }
  
  data = data.frame()
  new_tfb_spline(data, domain = domain, penalized = penalized,
                 global = global, resolution = resolution, ...)
  
}
