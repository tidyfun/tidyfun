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
#'   `order` (if it is a discrete variable) and labels for its levels? 
#'   Defaults to TRUE. Supply a list of arguments (grep source code for `order_ticks_args``) 
#'   to override default appearance of labels. 
#'   **Switch this off if you use facetting, it's a hack and will produce nonsense.**
#' @return a `ggplot2`` object
#' @export
#' @importFrom grid unit grobTree textGrob gpar 
#' @examples
#' \dontrun{
#' set.seed(1221)
#' data = expand.grid(group = factor(1:5), rep = 1:10)
#' data = dplyr::mutate(data, 
#'                      id = paste(group, rep, sep = "-"), 
#'                      f =  tf_rgp(50),
#'                      fb = tfb(f))
#' 
#' gglasagna(data, f, label = id)
#' gglasagna(data, fb, label = id, order = group)
#' # order is lowest first / on top by default
#' gglasagna(data, f, label = id, order = tf_depth(f))
#' gglasagna(data, f, label = id, order_by = first) +
#'   facet_wrap(~group, scales = "free")
#' # order of layers is by "order_by" within "order":
#' gglasagna(data, fb, label = id, order = group, order_by = first)
#' }
gglasagna <- function(data, y, order = NULL, label = NULL,
                      arg = NULL, order_by = NULL, 
                      order_ticks = TRUE) {
  order_ticks_args <- list(
    color = "black", 
    linetype = 2, 
    alpha = 0.5, 
    fontsize = theme_get()$text$size * .8, 
    rot = 90,
    label_offset = grid::unit(0.02, "npc")
  )
  if (is.list(order_ticks)) {
    order_ticks_args <- utils::modifyList(order_ticks_args, order_ticks)
    order_ticks <- TRUE
  }
  
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
    order <- bquote(..row)
    order_ticks <- FALSE
  }
  has_label <- !is.null(match.call()[["label"]])
  if (!has_label) {
    label <- bquote(names(.(enexpr(y))) %||% row_number())
    labelname <- ""
  } else {
    label <- match.call()$label
    labelname <- deparse(label)
  }
  y_name <- quo_name(enquo(y))
  data <- mutate(data, 
                 ..label = !!label, 
                 ..row = row_number(),
                 ..order = !!order)
  tf_eval <- suppressMessages(
      tf_unnest(data, y_name, .arg = arg, .sep = "___")
    ) %>%
    rename(..y = matches("___id"), 
           ..x = matches("___arg"), 
           ..fill = matches("___value"))
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
    if (!has_order) {
      tf_eval <- tf_eval %>% mutate(..order = ..order_by_value)
    }
  } else {
    order_by_label <- NULL
    tf_eval <- tf_eval %>% mutate(..order_by_value = ..row)
  }
  # create order of rows
  tf_eval <- tf_eval %>%
    arrange(..order, ..order_by_value, ..row) %>%
    mutate(..y = ordered(..y, levels = rev(unique(..y)))) %>%
   # mutate(..y = as.numeric(..y)) %>% 
    rename(!!y_name := ..fill)
  labeldata <- with(tf_eval, 
                  data_frame(breaks = unique(..y), 
                             labels = ..label[!duplicated(..y)]))

  p <- ggplot(tf_eval) +
    geom_tile(aes(y = ..y, x = ..x, fill = !!sym(y_name))) +
    xlab("")
  if (!is.null(order_label) | !is.null(order_by_label)) {
    p <- p + labs(caption = paste(
      "ordered by:",
      paste0(
        order_label, ifelse(has_order_by & has_order, "; ", ""),
        order_by_label
      )
    ))
  }
  if (!is.null(order_ticks) & is.discrete(pull(tf_eval, ..order))) {
     order_ticks_data <- data %>%
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
         data = order_ticks_data, aes(yintercept = tick_pos),
         col = order_ticks_args$color, alpha = order_ticks_args$alpha,
         linetype = order_ticks_args$linetype
       )
     # adding a secondary axis with labels for the order-variable only works
     # if we make the y-axis continuous -- but a) it isn't,really and 
     # b)scales="free" is needed for good looking facets but won't 
     # close gaps for pseudo-continuous y-scales, 
     # so we use this annotation_custom weirdness instead
     order_ticks_labelpos <- with(order_ticks_data,
                                  grid::unit(label_pos/max(tick_hi), "npc"))
     order_ticks_labels <- grid::grobTree(
       grid::textGrob(order_ticks_data %>% pull(!!order), 
                      x = order_ticks_args$label_offset, 
                      y = order_ticks_labelpos, 
                      rot = order_ticks_args$rot, 
                      gp = grid::gpar(fontsize = order_ticks_args$fontsize)))
     p <- p + annotation_custom(order_ticks_labels) 
   } 
  p <- p + scale_y_discrete(labelname, breaks = labeldata$breaks, 
                         labels = labeldata$labels)
  # TODO: use another geom_line or _tile to add a colour bar for order_by_values
  p
}
