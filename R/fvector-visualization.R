#' @import ggplot2
#' @importFrom modelr seq_range
#' @export
ggplot.fvector <- function(f, argvals, n_grid = 50, points = TRUE) {
  if (missing(argvals)) {
    argvals <-  if (!isTRUE(n_grid > 1)) {
     tidyfun::argvals(f)
    }  else {
      map2(list(modelr::seq_range(domain(f), n = n_grid)), 
          ensure_list(tidyfun::argvals(f)), union) %>% 
      adjust_resolution(f) %>% map(sort)
    }
  }
  d <- as.data.frame(f, argvals = argvals, interpolate = TRUE)
  p <- ggplot(d, aes(x = argvals, y = data, group = id)) + 
    geom_line(alpha = max(.05, 2/length(f)))
  if (points) {
    p + geom_point(data = as.data.frame(f, argvals = argvals(f)),
      alpha = max(.05, 2/length(f)))
  } else p
}


# TOD0:
# spaghettiplot
# lasagnaplot
# geom_spaghetti 
# geom_lasagna
