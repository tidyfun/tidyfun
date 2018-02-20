#' @import ggplot2
#' @importFrom modelr seq_range
#' @export
ggplot.fvector <- function(f, argvals, n_grid = 50, points = is_feval(f), 
  type = c("spaghetti", "lasagna")) {
  type <- match.arg(type)
  if (missing(argvals)) {
    argvals <-  if (!isTRUE(n_grid > 1)) {
     tidyfun::argvals(f)
    }  else {
      map2(list(modelr::seq_range(domain(f), n = n_grid)), 
          ensure_list(tidyfun::argvals(f)), union) %>% 
      adjust_resolution(f) %>% map(sort)
    }
  }
  d <- if (is_feval(f)) {
    as.data.frame(f, argvals = argvals, interpolate = TRUE) 
  } else as.data.frame(f, argvals = argvals)
  if (type == "spaghetti") {
    p <- ggplot(d, aes(x = argvals, y = data, group = id)) + 
      geom_line(alpha = max(.05, 2/length(f)))
    if (points) {
      p + geom_point(data = as.data.frame(f, argvals = argvals(f)),
        alpha = max(.05, 2/length(f)))
    } else p
  } 
  if (type == "lasagna") {
    ggplot(d, aes(y = id, x = argvals, fill =  data, group = id)) + 
      geom_tile(colour = NA) +
      scale_fill_gradient2(deparse(substitute(f)), midpoint = median(d$data))
  }
}

#' @importFrom utils modifyList
#' @export
plot.fvector <- function(f, argvals, n_grid = 50, points = is_feval(f), 
  type = c("spaghetti", "lasagna"), ...) {
  type <- match.arg(type)
  if (missing(argvals)) {
    argvals <-  if (!isTRUE(n_grid > 1)) {
      tidyfun::argvals(f)
    }  else {
      map2(list(modelr::seq_range(domain(f), n = n_grid)), 
        ensure_list(tidyfun::argvals(f)), union) %>% 
        adjust_resolution(f) %>% map(sort)
    }
  }
  m <- if (is_feval(f)) {
    as.matrix(f, argvals = argvals, interpolate = TRUE) 
  } else as.matrix(f, argvals = argvals)
  x <- drop(attr(m, "argvals"))
  if (type == "spaghetti") {
    args <- modifyList(
      list(x = drop(attr(m, "argvals")), y = t(m), type = ifelse(points, "b", "l"), 
        ylab = deparse(substitute(f)), xlab = "", lty = 1,
        col = rgb(0,0,0, max(.05, 2/length(f))), pch = 19), 
      list(...))
    do.call(matplot, args)
  }  
  if (type == "lasagna") {
    args <- modifyList(
      list(x = drop(attr(m, "argvals")), y = seq_len(nrow(m)), 
        z = t(m), col = heat.colors(25),
        ylab = "id", xlab = "", yaxt = "n"), 
      list(...))
    do.call(image, args)
    axis(2, at = seq_len(nrow(m)), labels = names(f))
  }
  invisible(f)
}


# TOD0:
# spaghettiplot
# lasagnaplot
# geom_spaghetti 
# geom_lasagna
