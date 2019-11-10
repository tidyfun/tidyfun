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



fit_wavelet <- function(data, threshold_args, wd_args, levels, arg_u, regular) {
  eval_list <- split(data$data, data$id)
  index_list <- split(attr(arg_u, "index"), data$id)
  
  coefs <- lapply(eval_list, wd, wd_args)
  
  if (nlevelsWT(coefs[[1]]) - 1 < levels) levels <- nlevelsWT(coefs[[1]]) - 1
  
  coefs <- lapply(coefs, wavethresh::threshold.wd, 
                  threshold_args, levels = levels)
  
  fit <- lapply(coefs, wr)
  list(fit = fit, wd_coefs = coefs)
}
