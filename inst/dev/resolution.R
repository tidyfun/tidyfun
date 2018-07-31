get_resolution <- function(arg) {
  min_diff = map(arg, ~ min(diff(.x))) %>% unlist %>% min
  10^(floor(log10(min_diff))-1)
}

domain <- c(0, 1)
resolution <- 0.01
arg <- c(0, 0.11, 0.111, 0.2, 0.351, 1)


