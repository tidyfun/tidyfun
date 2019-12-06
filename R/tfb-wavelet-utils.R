interpolate_arg <- function(arg_list) {
  # Interpolates a given input grid to the next dyadic power, so that the points
  # have equal distance. The lowest and highest point stay the same.
  n <- vapply(arg_list, length, numeric(1))
  closest_power <- round(log2(n))
  n_diff <- 2^closest_power - n
  
  interp_index <- map2(arg_list, 
                       n_diff,
                       function(x, y) {
                         slope <- (max(x) - min(x)) / (length(x) + y - 1)
                         seq(min(x), max(x), by = slope)
                       })
  unlist(interp_index)
}



fit_wavelet <- function(data, Z, penalized, glmnet_args) {
  eval_list <- split(data$value, data$id)
  
  if (!penalized) {
    qrZ <- qr(Z)
    coefs <- map(eval_list, function(x) {
      coefs <- qr.coef(qrZ, x)
      # deal with rank deficient qr:
      coefs[is.na(coefs)] <- 0
      coefs
    })
    
  } else {
    
    coefs <- map(eval_list, 
                 function(y) {
                   temp_model <- do.call(cv.glmnet, c(list(Z, y), 
                                                              glmnet_args))
                   as.numeric(coefficients(temp_model))
                 }
    )
  }
  coefs
}


fit_wavelet_irr <- function(data, Z, penalized, glmnet_args, arg_u) {
  # Difference to fit_wavelet() is that only the rows in Z are used that were in
  # the input grid of the original curve
  eval_list <- split(data$value, data$id)
  index_list <- split(attr(arg_u, "index"), data$id)
  
  if (!penalized) {
    coefs <- map2(index_list, eval_list,
                  function(x, y) {
                    qrZ <- qr(Z[x, ])
                    # Least squares fit, directly computed
                    coefs <- qr.coef(qrZ, y)
                    # deal with rank deficient qr:
                    coefs[is.na(coefs)] <- 0
                    coefs
                  }
    )
  } else {
    coefs <- map2(x = index_list, y = eval_list,
                  function(x, y) {
                    Z <- Z[x, ]
                    temp_model <- do.call(cv.glmnet, c(list(Z, y), 
                                                               glmnet_args))
                    as.numeric(coefficients(temp_model))
                  }
    )
  }
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
  
  # Check that x within support limits:
  # 
  # if (any(x<range.x[1])|any(x>range.x[2]))
  #   stop("All abscissae should be within range.x values.")
  # 
  # # Ensure that the number of levels is `allowable'.
  # 
  # if (!any(numLevels==(1:10)))
  #   stop("Number of levels should be between 2 and 10.")
  # 
  # # Ensure the resolution value is `allowable'.
  # 
  # if (!any(resolution==(2^(10:20))))
  #   stop("Resolution value should be a power of 2, with the
  #             power between 10 and 20.")
  
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
    
    putCobj <- putC.wd(wdObj,level=0,v=0)
    putCobj$D <- putCobj$D*0
    putCobj$D[resolution-k] <- 1
    
    # Obtain kth column of Z via linear interpolation
    # of the wr(putCobj) grid values:
    
    wtVec <- xUres - fXuRes
    wvVec <- wr.wd(putCobj)
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
  # interpolates the input matrix at the new grid defined by arg_new
  Xnew <- bind_cols(apply(X, 2, function(x) approx(arg_old, x, xout = arg_new)))
  Xnew <- unname(as.matrix(Xnew[, grepl("y", colnames(Xnew))]))
  Xnew
}
