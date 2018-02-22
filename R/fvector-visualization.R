prep_plotting_argvals <- function(f, n_grid) {
  if (!isTRUE(n_grid > 1)) {
    tidyfun::argvals(f)
  }  else {
    map2(list(modelr::seq_range(domain(f), n = n_grid)), 
      ensure_list(tidyfun::argvals(f)), union) %>% 
      adjust_resolution(f) %>% map(sort)
  }
}

#' Visualization functions for `fvector`s
#' 
#' Some `base` and `ggplot`-functions for displaying functional data in 
#' spaghetti- (i.e., line plots) and lasagna- (i.e., heat map) flavors. 
#' 
#' `funplot` uses `ggplot2`, the others use the `base`-system.
#' 
#' If no `argvals` are provided, evaluation points (`argvals`) for the functions 
#' are given by the union of the `fvector`'s `argvals` and an equidistant grid 
#' over its domain with `n_grid` points. If you want to only see the original 
#' data for `feval`-objects without inter-/extrapolation, use `n_grid < 1` or 
#' `n_grid = NA`.
#'    
#' @param f an `fvector` object
#' @param argvals (optional) numeric vector to be used as `argvals`. See details.
#' @param n_grid minimal size of equidistant grid used for plotting, 
#'   defaults to 50. See details.
#' @param points should the original evaluation points be marked by points?
#'   Defaults to `TRUE` for `feval`- and FALSE for `fbase`-objects
#' @param type "spaghetti": line plots, "lasagna": heat maps.
#' @param alpha [alpha-value](grDevices::rgb()) for noodle transparency. 
#'   Defaults to 2/(no. of observations). Lower is more transparent.
#' @return for `funplot`: the `ggplot`-object for further modification. For the 
#'  others: the plotted `fvector`-object, invisibly.
#' @import ggplot2
#' @importFrom modelr seq_range
#' @export
#' @rdname fvectorviz
funplot <- function(f, argvals, n_grid = 50, points = is_feval(f), 
  type = c("spaghetti", "lasagna"), alpha =  min(1, max(.05, 2/length(f)))) {
  assert_class(f, "fvector")
  assert_number(n_grid, na.ok = TRUE)
  assert_flag(points)
  type <- match.arg(type)
  if (missing(argvals)) {
    argvals <- prep_plotting_argvals(f, n_grid)
  }
  d <- if (is_feval(f)) {
    as.data.frame(f, argvals = argvals, interpolate = TRUE) 
  } else {
    as.data.frame(f, argvals = argvals)
  }
  
  if (type == "spaghetti") {
    p <- ggplot(d, aes(x = argvals, y = data, group = id)) + 
      geom_line(alpha = alpha)
    if (points) {
      p <- p + 
        geom_point(data = as.data.frame(f, argvals = argvals(f)), alpha = alpha)
    } 
  } 
  if (type == "lasagna") {
    d <- d %>% 
      mutate(id_num = as.numeric(as.factor(id))) %>% 
      group_by(id) %>% 
      mutate(xmax = c(argvals[-1], max(argvals) + mean(diff(argvals))))
    p <- ggplot(d, aes(y = id_num, xmin = argvals, xmax = xmax, 
      ymin = id_num - .5, ymax = id_num + .5, fill = data)) + 
      geom_rect(colour = NA) +
      scale_fill_gradient2(deparse(substitute(f)), 
        midpoint = median(d$data, na.rm = TRUE)) + 
      scale_y_continuous(name = "id", breaks = 1:length(f), labels = names(f))
  }
  p
}

#' @param x an `fvector` object
#' @param y (optional) numeric vector to be used as `argvals` (i.e., for the **x**-axis...!)
#' @param ... additional arguments for [matplot()] ("spaghetti") or [image()] ("lasagna")
#' @importFrom utils modifyList
#' @importFrom graphics matplot image axis
#' @importFrom grDevices heat.colors rgb
#' @export
#' @rdname fvectorviz
#' @references Swihart, B. J., Caffo, B., James, B. D., Strand, M., Schwartz, B. S., & Punjabi, N. M. (2010). 
#' Lasagna plots: a saucy alternative to spaghetti plots. *Epidemiology (Cambridge, Mass.)*, **21**(5), 621-625.
plot.fvector <- function(x, y, n_grid = 50, points = is_feval(x), 
  type = c("spaghetti", "lasagna"), alpha = min(1, max(.05, 2/length(x))), ...) {
  type <- match.arg(type)
  assert_logical(points)
  assert_number(n_grid, na.ok = TRUE)
  f <- x
  if (missing(y)) {
    argvals <- prep_plotting_argvals(x, n_grid)
  } else {
    argvals <- y
  }  
  m <- if (is_feval(f)) {
    as.matrix(f, argvals = argvals, interpolate = TRUE) 
  } else as.matrix(f, argvals = argvals)
  if (type == "spaghetti") {
    args <- modifyList(
      list(x = drop(attr(m, "argvals")), y = t(m), type = ifelse(points, "b", "l"), 
        ylab = deparse(substitute(x)), xlab = "", lty = 1,
        col = rgb(0,0,0, alpha), pch = 19), 
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

#' @importFrom graphics matlines
linespoints_fvector <- function(x, argvals, n_grid, points, alpha, ...) {
  assert_number(n_grid, na.ok = TRUE)
  if (missing(argvals)) {
    argvals <- prep_plotting_argvals(x, n_grid)
  }
  m <- if (is_feval(x)) {
    as.matrix(x, argvals = argvals, interpolate = TRUE) 
  } else as.matrix(x, argvals = argvals)
  args <- modifyList(
    list(x = drop(attr(m, "argvals")), y = t(m), type = ifelse(points, "p", "l"), 
      lty = 1, col = rgb(0,0,0, alpha), pch = 19), 
    list(...))
  do.call(matlines, args)
}

#' @export
#' @rdname fvectorviz
lines.fvector <- function(x, argvals, n_grid = 50, 
  alpha = min(1, max(.05, 2/length(x))), ...) {
  args <- c(as.list(match.call())[-1], points = FALSE)
  do.call(linespoints_fvector, args)
  invisible(x)
}
#' @export
#' @rdname fvectorviz
points.fvector <- function(x, argvals, n_grid = 50, 
    alpha = min(1, max(.05, 2/length(x))), ...) {
  args <- c(as.list(match.call())[-1], points = TRUE)
  do.call(linespoints_fvector, args)
  invisible(x)
}

# TOD0:
# geom_spaghetti 
# geom_lasagna
