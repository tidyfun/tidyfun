check_dyadic <- function(n) {
  dyadic_params <- list()
  dyadic_params[["dyadic"]] <- round(log2(n)) == log2(n)
  dyadic_params[["next_power"]] <- ceiling(log2(n))
  dyadic_params[["previous_power"]] <- floor(log2(n))
  dyadic_params
}

check_spacing <- function(arg) {
  spacing_params <- list()
  arg_range <- range(arg)
  first_dist <- arg[2] - arg[1]
  ideal_seq <- seq(arg_range[1], arg_range[2], by = first_dist)
  # suppress warnings because possible unequal length
  spacing_params[["equal_spacing"]] <- suppressWarnings(all(ideal_seq == arg))
  spacing_params[["first_offender"]] <- suppressWarnings(
    which(ideal_seq != arg)[1])
  spacing_params
}

grid_adjustment <- function(data, dyadic_params, spacing_params) {
  # # very simple zero padding
  # eval_list <- split(data$data, data$id)
  # map2()
  # wavelets::extend.series(data)
  data
}



fit_wavelet <- function(data, threshold_args, wd_args, arg_u, regular) {
  eval_list <- split(data$data, data$id)
  index_list <- split(attr(arg_u, "index"), data$id)
  
  coefs <- lapply(eval_list, wd, wd_args)
  
  if (nlevelsWT(coefs[[1]]) - 1 < threshold_args$levels) {
    threshold_args$levels <- nlevelsWT(coefs[[1]]) - 1
    warning(paste0("levels input is too big for the data. Using levels = ", 
                   threshold_args$levels))
  }
  
  coefs <- lapply(coefs, wavethresh::threshold.wd, unlist(threshold_args))
  
  fit <- lapply(coefs, wr)
  list(fit = fit, wd_coefs = coefs)
}


########## R function: ZDaub ##########

# Creates a Daubechies wavelet basis function
# design matrix for a given input vector "x".

# Last changed: 09 SEP 2011
# From https://projecteuclid.org/euclid.ejs/1323785605#supplemental

ZDaub <- function(x,range.x=range(x),numLevels=6,filterNumber=5,
                  resolution=16384)
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

