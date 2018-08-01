prep_plotting_arg <- function(f, n_grid) {
  if (!isTRUE(n_grid > 1)) {
    tidyfun::arg(f)
  }  else {
    modelr::seq_range(domain(f), n = n_grid) %>% 
      round_resolution(attr(f, "resolution")) %>%
      setdiff(round_resolution(tidyfun::arg(f), attr(f, "resolution"))) %>% 
      union(unlist(tidyfun::arg(f))) %>% sort
  }
}

#' Visualization functions for `tf`s
#' 
#' Some `base` and `ggplot`-functions for displaying functional data in 
#' spaghetti- (i.e., line plots) and lasagna- (i.e., heat map) flavors. 
#' 
#' `funplot` uses `ggplot2`, the others use the `base`-system.
#' 
#' If no `arg` are provided, evaluation points (`arg`) for the functions 
#' are given by the union of the `tf`'s `arg` and an equidistant grid 
#' over its domain with `n_grid` points. If you want to only see the original 
#' data for `tfd`-objects without inter-/extrapolation, use `n_grid < 1` or 
#' `n_grid = NA`.
#'    
#' @param f an `tf` object
#' @param arg (optional) numeric vector to be used as `arg`. See details.
#' @param n_grid minimal size of equidistant grid used for plotting, 
#'   defaults to 50. See details.
#' @param points should the original evaluation points be marked by points?
#'   Defaults to `TRUE` for irregular `tfd` and FALSE for all others
#' @param type "spaghetti": line plots, "lasagna": heat maps.
#' @param alpha alpha-value (see[grDevices::rgb()]) for noodle transparency. 
#'   Defaults to 2/(no. of observations). Lower is more transparent.
#' @return for `funplot`: the `ggplot`-object for further modification. For the 
#'  others: the plotted `tf`-object, invisibly.
#' @import ggplot2
#' @importFrom modelr seq_range
#' @export
#' @rdname tfviz
funplot <- function(f, arg, n_grid = 50, points = is_irreg(f), 
  type = c("spaghetti", "lasagna"), alpha =  min(1, max(.05, 2/length(f)))) {
  assert_class(f, "tf")
  assert_number(n_grid, na.ok = TRUE)
  assert_flag(points)
  type <- match.arg(type)
  if (missing(arg)) {
    arg <- prep_plotting_arg(f, n_grid)
  }
  d <- if (is_tfd(f)) {
    as.data.frame(f, arg = arg, interpolate = TRUE) 
  } else {
    as.data.frame(f, arg = arg)
  }
  
  if (type == "spaghetti") {
    p <- ggplot(d, aes(x = arg, y = data, group = id)) + 
      geom_line(alpha = alpha)
    if (points) {
      p <- p + 
        geom_point(data = as.data.frame(f, arg = arg(f)), alpha = alpha)
    } 
  } 
  if (type == "lasagna") {
    d <- d %>% 
      mutate(id_num = as.numeric(as.factor(id))) %>% 
      group_by(id) %>% 
      mutate(xmax = c(arg[-1], max(arg) + mean(diff(arg))))
    p <- ggplot(d, aes(y = id_num, xmin = arg, xmax = xmax, 
      ymin = id_num - .5, ymax = id_num + .5, fill = data)) + 
      geom_rect(colour = NA) +
      scale_fill_gradient2(deparse(substitute(f)), 
        midpoint = median(d$data, na.rm = TRUE)) + 
      scale_y_continuous(name = "id", breaks = 1:length(f), labels = names(f))
  }
  p
}

#' @param x an `tf` object
#' @param y (optional) numeric vector to be used as `arg` (i.e., for the **x**-axis...!)
#' @param ... additional arguments for [matplot()] ("spaghetti") or [image()] ("lasagna")
#' @importFrom utils modifyList
#' @importFrom graphics matplot image axis
#' @importFrom grDevices heat.colors rgb
#' @export
#' @rdname tfviz
#' @references Swihart, B. J., Caffo, B., James, B. D., Strand, M., Schwartz, B. S., & Punjabi, N. M. (2010). 
#' Lasagna plots: a saucy alternative to spaghetti plots. *Epidemiology (Cambridge, Mass.)*, **21**(5), 621-625.
plot.tf <- function(x, y, n_grid = 50, points = is_irreg(x), 
  type = c("spaghetti", "lasagna"), alpha = min(1, max(.05, 2/length(x))), ...) {
  type <- match.arg(type)
  assert_logical(points)
  assert_number(n_grid, na.ok = TRUE)
  if (missing(y)) {
    arg <- prep_plotting_arg(x, n_grid)
    # irreg args need to be turned to a vector for as.matrix below:
    if (is.list(arg)) arg <- sort(unique(unlist(arg)))
  } else {
    arg <- y
  }  
  m <- if (is_tfd(x)) {
    as.matrix(x, arg = arg, interpolate = TRUE) 
  } else as.matrix(x, arg = arg)
  if (type == "spaghetti") {
    args <- modifyList(
      list(x = drop(attr(m, "arg")), y = t(m), type = "l", 
        ylab = deparse(substitute(x)), xlab = "", lty = 1,
        col = rgb(0,0,0, alpha)), 
      list(...))
    do.call(matplot, args)
    if (points) {
       pointsargs <- modifyList(
        list(x = x, arg = NULL,
          n_grid = NA, points = TRUE, interpolate = FALSE,
          pch = 19, ol = rgb(0,0,0, alpha)), 
        list(...))
      do.call(linespoints_tf, pointsargs)
    }
  }  
  if (type == "lasagna") {
    args <- modifyList(
      list(x = drop(attr(m, "arg")), y = seq_len(nrow(m)), 
        z = t(m), col = heat.colors(25),
        ylab = "id", xlab = "", yaxt = "n"), 
      list(...))
    m <- m[rev(seq_len(nrow(m))), ] #so first obs is on top
    do.call(image, args)
    axis(2, at = seq_len(nrow(m)), labels = rev(names(x) %||% seq_len(nrow(m))))
  }
  invisible(x)
}

#' @importFrom graphics matlines
linespoints_tf <- function(x, arg, n_grid = 50, points = TRUE, 
  alpha = min(1, max(.05, 2/length(x))), interpolate = TRUE, ...) {
  assert_number(n_grid, na.ok = TRUE)
  if (missing(arg)) {
    arg <- prep_plotting_arg(x, n_grid)
    # irreg args need to be turned to a vector for as.matrix below:
    if (is.list(arg)) arg <- sort(unique(unlist(arg)))
  }
  m <- if (is_tfd(x)) {
    suppressWarnings(
      as.matrix(x, arg = arg, interpolate = interpolate))
  } else as.matrix(x, arg = arg)
  args <- modifyList(
    list(x = drop(attr(m, "arg")), y = t(m), type = ifelse(points, "p", "l"), 
      lty = 1, col = rgb(0,0,0, alpha), pch = 19), 
    list(...))
  do.call(matlines, args)
}

#' @export
#' @rdname tfviz
lines.tf <- function(x, arg, n_grid = 50, 
  alpha = min(1, max(.05, 2/length(x))), ...) {
  args <- c(modifyList(head(formals(lines.tf), -1),
    as.list(match.call())[-1]), points = FALSE)
  # eval here so pipe finds it later
  args$x <- x
  do.call(linespoints_tf, args)
  invisible(x)
}
#' @export
#' @rdname tfviz
#' @param interpolate should functions be evaluated (i.e., inter-/extrapolated)
#'   for arg for which no original data is available? Only relevant for
#'   tfd, defaults to FALSE
points.tf <- function(x, arg, n_grid = NA, 
    alpha = min(1, max(.05, 2/length(x))), interpolate = FALSE, ...) {
  args <- c(modifyList(head(formals(points.tf), -1),
    as.list(match.call())[-1]), points = TRUE)
  # eval here so pipe finds it later
  args$x <- x
  do.call(linespoints_tf, args)
  invisible(x)
}
