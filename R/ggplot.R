#' Geoms for `tf` objects
#' 
#' Defines spaghetti and lasagna plot geoms for `tf`-objects.  
#' `geom_spaghetti` does spaghetti plots, `geom_meatballs` does spaghetti plots
#' with points for the actual evaluations, `geom_lasagna` does lasagna plots. 
#' All of this is a hack using `stat_tf` and subject to change.
#' 
#' @section `tf` aesthetic:
#'   Mandatory. Used to designate a column of class `tf` to be visualized. 
#' @section `order` aesthetic:
#'   optional. Used to designate a variable that defines the (low-to-high) ordering of the
#'   functions in the lasagna plot. See also: `order_by`
#' @examples
#' set.seed(1221)
#' data = data.frame(col = sample(gl(5, 2)))
#' data$f = rgp(10)
#' data$fi = jiggle(data$f)
#' data$fb = tfb(data$f)
#' library(ggplot2)
#' ggplot(data, aes(tf = f, color = depth(f))) + geom_spaghetti() + 
#'   annotate("text", x = 1.05, y = drop(data$f[, 1]), label = 1:nrow(data))
#' ggplot(data, aes(tf = fi, shape = col, color = col)) + geom_meatballs()
#' ggplot(data, aes(tf = fi)) + geom_meatballs(spaghetti = FALSE) + 
#'   facet_wrap(~col)
#' 
#' # geom_lasagna is a hack of geom_line because geom_tile won't accept the 
#' # id-factor as a vertical axis -- adjust line width ("size") to get a proper 
#' # lasagna without gaps just like nonna used to make back in the old country:
#' ggplot(data, aes(tf = fb, order = col)) + geom_lasagna(size = 8)
#' ggplot(data, aes(tf = fb, order = col)) + geom_lasagna(size = 12)
#' 
#' # use the order-aesthetic to define the vertical ordering of the functions 
#' # (low values - low positions)
#' ggplot(data, aes(tf = f, order = -depth(f))) + geom_lasagna(size = 8)
#' # or use order_by to define an ordering computed directly 
#' # on each function's evaluations:
#' ggplot(data, aes(tf = f)) + geom_lasagna(order_by = mean)
#' ggplot(data, aes(tf = f)) + geom_lasagna(order_by = min)
#' ggplot(data, aes(tf = f)) + geom_lasagna(order_by = function(f) f[1])
#' # last one same as this:
#' ggplot(data, aes(tf = f, order = drop(f[, 0]))) + geom_lasagna()
#' # combine the two: 
#' ggplot(data, aes(tf = f, order = col)) + geom_lasagna(order_by = function(f) f[1]) 
#' # .. but facetting is broken for lasagna:
#' ggplot(data, aes(tf = f, order = col)) + 
#'   geom_lasagna(order_by = function(f) f[1])  + facet_wrap(~ col)   
#' @name ggtf
NULL

is.finite.tfd <- function(x) map(evaluations(x), ~ all(is.finite(x)))
scale_type.tf <- function(x) "identity"

#' @export
#' @importFrom ggplot2 ggproto Stat Geom
#' @rdname ggtf
#' @usage NULL
#' @format NULL
StatTf <- ggproto("StatTf", Stat,
  required_aes = "tf",
  default_aes = aes(x = stat(.arg), y = stat(.value), group = stat(.id), 
    order = NULL),
  setup_params = function(data, params) {
    if (!is.null(params$arg))
      return(params)
    params$arg <- arg(pull(data, tf))
    if (!is.null(params$order_by)) 
      stopifnot(is.function(params$order_by))
    params
  },
  compute_layer = function(self, data, params, layout) {
    stopifnot(is_tf(pull(data, tf)))
    tf_eval <- evaluate(object = data, arg = params$arg, tf) %>%
      select(-group) %>% 
      unnest(.id = ".id") %>% 
      rename(.arg = arg, .value = value) 
    if (is.null(data$order) & is.null(params$order_by)) {
      ordered_id <- tf_eval %>% pull(.id) %>% unique
    } 
    if (is.null(data$order) & !is.null(params$order_by)) {
      ordered_id <- tf_eval %>% 
        group_by(.id) %>% 
        summarize(order = params$order_by(.value)) %>%
        arrange(order) %>% pull(.id)
    } 
    if (!is.null(data$order) & is.null(params$order_by)) {
      ordered_id <- tf_eval %>% arrange(order) %>% pull(.id) %>% unique
    }
    if (!is.null(data$order) & !is.null(params$order_by)) {
      ordered_id <- tf_eval %>% 
        group_by(.id) %>% 
        summarize(
          order2 = ordered(params$order_by(.value)), 
          order = order[1]) %>% 
        arrange(order, order2) %>% pull(.id) %>% unique
    }
    tf_eval <- mutate(tf_eval, .id = ordered(.id, ordered_id))
    tf_eval
  },
  # need this so arg, order_by gets recognized as valid parameters
  # because layer() only checks compute_panel & compute_group
  compute_panel = function(self, data, scales, arg, order_by) {
    browser()
    Stat$compute_panel(self, data, scales)
  }  
)

#' @export
#' @rdname ggtf
#' @inheritParams ggplot2::stat_identity
#' @param na.rm remove NAs? defaults to `TRUE`
stat_tf <- function(mapping = NULL, data = NULL, geom = "line",
  position = "identity", na.rm = TRUE, show.legend = NA, 
  inherit.aes = TRUE, arg = NULL, ...) {
  layer(
    stat = StatTf, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arg = arg, ...)
  )
}

# geom --------------------------------------------------------------------

#' @export
#' @rdname ggtf
#' @format NULL
#' @param arg where to evaluate `tf` -- defaults to the default ;)
geom_spaghetti <- function(mapping = NULL, data = NULL,
  position = "identity", na.rm = TRUE, show.legend = NA, 
  inherit.aes = TRUE, arg = NULL, ...) {
  layer(
    stat = StatTf, data = data, mapping = mapping, geom = "line", 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arg = arg, ...)
  )
}
#' @export
#' @rdname ggtf
#' @format NULL
#' @param spaghetti plot noodles along with meatballs? defaults to true.
geom_meatballs <- function(mapping = NULL, data = NULL,
  position = "identity", na.rm = TRUE, show.legend = NA, 
  inherit.aes = TRUE, arg = NULL, spaghetti = TRUE, ...) {
  list(
    layer(
    stat = StatTf, data = data, mapping = mapping, geom = "point", 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arg = arg, ...)),
    if (spaghetti) {
      layer(
        stat = StatTf, data = data, mapping = mapping, geom = "line", 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, arg = arg, ...))
    } else NULL) 
}
#' @export
#' @rdname ggtf
#' @format NULL
#' @param order_by a function that returns a single value when applied to a numeric vector,
#'  used to define the vertical ordering of the functions in the plot (see Examples)
#' @param size width for lasagna layers. You will need to set this manually, sorry.
geom_lasagna <- function(mapping = list(), 
  data = NULL,  position = "identity", na.rm = TRUE, show.legend = NA, 
  inherit.aes = TRUE, arg = NULL, order_by = NULL, size = 4, ...) {
  default_mapping <- 
    aes(x = stat(.arg), y = stat(.id), colour = stat(.value))
  mapping <- structure(modifyList(mapping, default_mapping), class = "uneval")
  layer(
    stat = StatTf, data = data, mapping = mapping, geom = "line",
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arg = arg, size = size, 
      order_by = order_by)
  ) 
}




   
   
  
