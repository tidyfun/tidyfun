check_dyadic <- function(n) {
  dyadic_params <- list()
  dyadic_params[["dyadic"]] <- round(log2(n)) == log2(n)
  dyadic_params[["closest_power"]] <- round(log2(n))
  dyadic_params[["next_power"]] <- ceiling(log2(n))
  dyadic_params[["n_diff"]] <- 2^dyadic_params[["closest_power"]] - n
  dyadic_params[["n_next"]] <- 2^dyadic_params[["next_power"]] - n
  dyadic_params
}

check_spacing <- function(arg) {
  spacing_params <- list()
  arg <- ifelse(!is.list(arg), list(arg), arg)
  # suppress warnings because possible unequal length
  spacing_params[["equal_spacing"]] <- vapply(arg, function(x) 
    is_equidist_numeric(x), logical(1))
  spacing_params[["diff_arg"]] <- lapply(arg, function(x) diff(x)) 
  spacing_params
}

remove_slope <- function(x, y) {
  last_element <- length(x)
  slope <- (y[last_element] - y[1]) / (x[last_element] - x[1])
  intercept <- x[1] - slope * x[1]
  f <- intercept + slope * x
  y_desloped <- y - f
  y_desloped <- structure(y_desloped,
                          slope = f)
  y_desloped
}

grid_adjustment <- function(data, dyadic_params, spacing_params) {
  # get rid of global slope
  eval_list <- split(data$data, data$id)
  index_list <- split(data$arg, data$id)
  
  
  # if n k of off 2^J  or series not equally spaced use interpolation
  # if off a lot use padding
  
  
  
  # if (all(dyadic_params$dyadic & spacing_params$equal_spacing)) {
  #   
  # } else if (any(!dyadic_params$dyadic)) {
  #   eval_list <- map(eval_list, function(x) wavelets::extend.series(x,
  #                                                         method = "zeros",
  #                                                         length = "powerof2"))
  # }
  
  # if (normal) {
  #   data <- map2(index_list, eval_list, function(x, y) remove_slope(x,y))
  # } else if (padding) {
  #   eval_list <- map2(eval_list,
  #                     dyadic_params$n_next, 
  #                     function(x, y) c(x, rep(0, y)))
  #   index_list <- map2(index_list, 
  #                      spacing_params$diff_arg, 
  #                      function(x, y) c(x, 
  #                                       seq(max(x) + max(y),
  #                                           dyadic_params$n_next,
  #                                           by = max(y))))
  #   data <- map2(index_list, eval_list, function(x, y) remove_slope(x,y))
  # } else if (interpolation) {
  #   interp_index <- map2(index_list, 
  #                        dyadic_params$n_diff,
  #                        function(x, y) {
  #                          slope <- (max(x) - min(x)) / (length(x) + y - 1)
  #                          seq(min(x), max(x), by = slope)
  #                        })
  #   index_list <- mapply(c, index_list, interp_index, SIMPLIFY = FALSE)
  #   # solve problem with n differently, since length doesnt stay same through 
  #   # interpolation
  #   map2(index_list, 
  #        eval_list, 
  #        function(x,y) {
  #          n <- length(x) / 2
  #          approx(x = x[1:n], y, xout = x[(n+1):2*n])$y 
  #          })
  # }
  
  
  data
}


fit_wavelet <- function(data, threshold_args, wd_args, arg_u, regular) {
  eval_list <- split(data$data, data$id)
  
  coefs <- map(eval_list, function(x) {
    wd_args$data <- x
    do.call(wd, wd_args, quote = TRUE)
  })
  
  if (nlevelsWT(coefs[[1]]) - 1 < threshold_args$levels) {
    threshold_args$levels <- nlevelsWT(coefs[[1]]) - 1
    warning(paste0("level input is too big for the data. Using levels = ", 
                   threshold_args$levels))
  }
  
  coefs <- map(coefs, function(x) {
    threshold_args$wd <- x
    do.call(threshold.wd, threshold_args, quote = TRUE)
  })
  
  fit <- map(coefs, wr)
  list(fit = fit, wd_coefs = coefs)
}


fit_wavelet_matrix <- function(data, Z) {
  eval_list <- split(data$data, data$id)
  
  Z_qr <- qr(Z)
  # least squares
  coefs <- map(eval_list, function(x) qr.coef(Z_qr, x))
  
  coefs
}


########## R function: ZDaub ##########

# Creates a Daubechies wavelet basis function
# design matrix for a given input vector "x".

# Last changed: 09 SEP 2011
# From https://projecteuclid.org/euclid.ejs/1323785605#supplemental

ZDaub <- function(x, range.x = range(x), numLevels = 6, filterNumber = 5,
                  resolution = 16384)
{
  # Load required package:
  
  library(wavethresh)
  
  # Check that x within support limits:
  
  if (any(x<range.x[1])|any(x>range.x[2]))
    stop("All abscissae should be within range.x values.")
  
  # Ensure that the number of levels is `allowable'.
  
  if (!any(numLevels==(1:10)))
    stop("Number of levels should be between 2 and 10.")
  
  # Ensure the resolution value is `allowable'.
  
  if (!any(resolution==(2^(10:20))))
    stop("Resolution value should be a power of 2, with the
              power between 10 and 20.")
  
  # Transform x to the unit interval and obtain variables
  # required for linear interpolation:
  
  xUnit <- (x - range.x[1])/(range.x[2] - range.x[1])
  xUres <- xUnit*resolution
  fXuRes <- floor(xUres)
  
  # Set filter and wavelet family  
  
  family <- "DaubExPhase"
  K <- 2^numLevels - 1
  
  # Create a dummy wavelet transform object
  
  wdObj <- wd(rep(0,resolution),filter.number=filterNumber,
              family="DaubExPhase")
  
  Z <- matrix(0,length(x),K)
  for (k in 1:K)
  {
    # Create wobj so that it contains the Kth basis
    # function of the Z matrix with `resolution' regularly 
    # spaced points:
    
    putCobj <- putC(wdObj,level=0,v=0)
    putCobj$D <- putCobj$D*0
    putCobj$D[resolution-k] <- 1
    
    # Obtain kth column of Z via linear interpolation
    # of the wr(putCobj) grid values:
    
    wtVec <- xUres - fXuRes
    wvVec <- wr(putCobj)
    wvVec <- c(wvVec,rep(wvVec[length(wvVec)],2))
    Z[,k] <- sqrt(resolution)*((1 - wtVec)*wvVec[fXuRes+1]
                               + wtVec*wvVec[fXuRes+2])
  }
  
  # Create column indices to impose "left-to-right" ordering
  # within the same level:
  
  newColInds <- 1
  for (ell in 1:(numLevels-1))
    newColInds <- c(newColInds,(2^(ell+1)-1):(2^(ell)))
  
  Z <- Z[,newColInds]
  
  return(Z)
}

############ End of ZDaub ###########

predict_matrix <- function(X, arg_old, arg_new) {
  t_X <- t(X)
  Xnew <- bind_cols(apply(t_X, 1, function(x) approx(arg_old, x, xout = arg_new)))
  
  Xnew <- Xnew %>% select(contains("y")) %>% as.matrix %>% unname %>% t()
  Xnew
}
