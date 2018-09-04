# from https://github.com/r-lib/scales/blob/master/R/scale-discrete.r
is.discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

#' Lasagna plots for `tf`s using `ggplot2`
#'
#' Lasagna plots show one color bar for each function.
#'
#' The vertical order of the lasagna layers is **increasing** in
#'
#' - `order` (if provided),
#' - the values returned by `order_by` (if provided),
#' - and the row number of the observations.
#'
#' i.e., lowest values are on top so that by default the first layer is the first
#' observation in `data` and the vertical order of the layers corresponds to the
#' ordering of observations for `arrange(data, order, order_by(value), row_number())`.
#'
#' @param data the data. Duhh.
#' @param y bare name of the `tf` column to visualize
#' @param order (optional) bare name of a column in `data` to define vertical
#'   order of lasagna layers.
#' @param label (optional) bare name of a column in `data` to define labels for
#'   lasagna layers. Defaults to names of `y`, if present, or row numbers.
#' @param arg `arg` to evaluate `y` om
#' @param order_by a function applied to each row in `y[, arg]` that must
#'   return a scalar value to define the order of lasagna layers.
#' @param order_ticks add horizontal lines indicating borders between levels of
#'   `order` (if discrete)? Defaults to true.
#' @param order_ticks_color defaults to "black"
#' @param order_ticks_linetype defaults to 2
#' @param order_ticks_alpha defaults to 0.5
#' @return a `ggplot2`` object
#' @export
#' @examples
#' \dontrun{
#' set.seed(1221)
#' data = expand.grid(group = 1:5, rep = 1:10)
#' data = dplyr::mutate(data, id = paste(group, rep, sep = "-"))
#' data$f = tf_rgp(50)
#' data$fi = tf_sparsify(data$f)
#' data$fb = tfb(data$f)
#' gglasagna(data, f, label = id)
#' gglasagna(data, f, label = id, order = group)
#' gglasagna(data, f, label = id, order = group)
#' gglasagna(data, f, label = id, order = tf_depth(f))
#' gglasagna(data, f, label = id, order_by = first) +
#'   facet_wrap(~group, scales = "free")}
gglasagna <- function(data, y, order = NULL, label = NULL,
                      arg = NULL, order_by = NULL, order_ticks = TRUE, order_ticks_color = "black",
                      order_ticks_linetype = 2, order_ticks_alpha = 0.5) {
  # FIXME: render errors for weird arg lenght (e.g. 93)
  stopifnot(is_tf(pull(data, !!enexpr(y))))
  has_order <- !is.null(match.call()[["order"]])
  has_order_by <- !is.null(match.call()[["order_by"]])
  order_label <- enexpr(order)
  if (has_order) {
    order_label <- quo_name(order_label)
    order <- match.call()$order
  } else {
    order_label <- NULL
    order <- bquote(rev(row_number()))
    order_ticks <- FALSE
  }
  if (is.null(label)) {
    label <- bquote(names(.(enexpr(y))) %||% row_number())
  } else {
    label <- match.call()$label
  }
  y_name <- quo_name(enquo(y))
  data <- mutate(data, ..label = !!label, ..order = !!order, ..row = row_number())
  tf_eval <- suppressMessages(tf_unnest(data, y_name, .arg = arg, .sep = "___")) %>%
    rename(..y = matches("___id"), ..x = matches("___arg"), ..fill = matches("___value"))
  order_by_label <- enexpr(order_by)
  if (has_order_by) {
    order_by_label <- quo_name(order_by_label)
    stopifnot(is.function(order_by))
    order_by_value <- tf_eval %>%
      group_by(..y) %>%
      summarize(..order_by_value = order_by(..fill)) %>%
      ungroup() %>%
      mutate(..order_by_value = rank(..order_by_value))
    tf_eval <- left_join(tf_eval, order_by_value, by = "..y")
    # override previous ordering
    if (is.null(match.call()[["order"]])) {
      tf_eval <- tf_eval %>% mutate(..order = ..order_by_value)
    }
  } else {
    order_by_label <- NULL
    tf_eval <- tf_eval %>% mutate(..order_by_value = ..order)
  }
  tf_eval <- tf_eval %>%
    arrange(..order, ..order_by_value, ..row) %>%
    mutate(..y = ordered(..y, levels = unique(..y), labels = unique(..label))) %>%
    rename(!!y_name := ..fill)

  p <- ggplot(tf_eval) +
    geom_tile(aes(y = ..y, x = ..x, fill = !!sym(y_name))) + ylab("") +
    xlab("")
  if (!is.null(order_label) | !is.null(order_by_label)) {
    p <- p + labs(caption = paste(
      "ordered by:",
      paste0(
        order_label, ifelse(has_order_by & has_order, ";", ""),
        order_by_label
      )
    ))
  }
  if (order_ticks & is.discrete(pull(tf_eval, ..order))) {
    order_ticks <- data %>%
      arrange(desc(!!order)) %>%
      mutate(ticks = row_number()) %>%
      group_by(!!order) %>%
      summarize(tick_hi = max(ticks), tick_lo = min(ticks)) %>%
      mutate(
        label_pos = (tick_hi + tick_lo) / 2,
        tick_pos = lead(tick_hi) + 0.5
      )
    p <- p +
      geom_hline(
        data = order_ticks, aes(yintercept = tick_pos),
        col = order_ticks_color, alpha = order_ticks_alpha,
        lty = order_ticks_linetype
      )
    #   p_ticks <- ggplot(order_ticks, aes(y = label_pos, label = !!order)) +
    #     geom_text(x = 0) + theme_void()
    #   #TODO: use gtable etc to align
  }
  # TODO: use another geom_line or _tile to add a colour bar for order_by_values
  p
}
