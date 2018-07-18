# cf. https://github.com/tidyverse/ggplot2/blob/69dfc4bc62828ecc3d1cda3dea4d0a664c4ea12c/R/sf.R
rm(list = ls())
devtools::load_all("~/fda/tidyfun")
is.finite.tfd <- function(x) map(evaluations(x), ~ all(is.finite(x)))
scale_type.tf <- function(x) "identity"
library(magrittr)
library(tidyverse)

#' @importFrom ggplot2 ggproto Stat Geom 
StatTfd <- ggproto("StatTfd", Stat,
  required_aes = "tfd",
  default_aes = aes(x = stat(.arg), y = stat(.value), group = stat(.id), 
    order = NULL),
  setup_params = function(data, params) {
    if (!is.null(params$arg))
      return(params)
    params$arg <- arg(pull(data, tfd))
    if (!is.null(params$order_by)) 
      stopifnot(is.function(params$order_by))
    params
  },
  compute_layer = function(self, data, params, layout) {
    tfd_eval <- evaluate(object = data, arg = params$arg, tfd) %>%
      select(-group) %>% 
      unnest(.id = ".id") %>% 
      rename(.arg = arg, .value = data) 
    if (is.null(data$order) & is.null(params$order_by)) {
      ordered_id <- tfd_eval %>% pull(.id) %>% unique
    } 
    if (is.null(data$order) & !is.null(params$order_by)) {
      ordered_id <- tfd_eval %>% 
        group_by(.id) %>% 
        summarize(order = params$order_by(.value)) %>%
        arrange(order) %>% pull(.id)
    } 
    if (!is.null(data$order) & is.null(params$order_by)) {
      ordered_id <- tfd_eval %>% arrange(order) %>% pull(.id) %>% unique
    }
    if (!is.null(data$order) & !is.null(params$order_by)) {
      ordered_id <- tfd_eval %>% 
        group_by(.id) %>% 
        summarize(
          order2 = ordered(params$order_by(.value)), 
          order = order[1]) %>% 
        arrange(order, order2) %>% pull(.id) %>% unique
    }
    tfd_eval <- mutate(tfd_eval, .id = ordered(.id, ordered_id))
    tfd_eval
  },
  # need this so arg, order_by gets recognized as valid parameters
  # because layer() only checks compute_panel & compute_group
  compute_panel = function(self, data, scales, arg, order_by) {
    browser()
    Stat$compute_panel(self, data, scales)
  }  
)
stat_tfd <- function(mapping = NULL, data = NULL, geom = "line",
  position = "identity", na.rm = FALSE, show.legend = NA, 
  inherit.aes = TRUE, arg = NULL, ...) {
  layer(
    stat = StatTfd, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arg = arg, ...)
  )
}

geom_spaghetti <- function(mapping = NULL, data = NULL,
  position = "identity", na.rm = FALSE, show.legend = NA, 
  inherit.aes = TRUE, arg = NULL, ...) {
  layer(
    stat = StatTfd, data = data, mapping = mapping, geom = "line", 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arg = arg, ...)
  )
}
geom_meatballs <- function(mapping = NULL, data = NULL,
  position = "identity", na.rm = FALSE, show.legend = NA, 
  inherit.aes = TRUE, arg = NULL, spaghetti = TRUE, ...) {
  list(
    layer(
    stat = StatTfd, data = data, mapping = mapping, geom = "point", 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arg = arg, ...)),
    if (spaghetti) {
      layer(
        stat = StatTfd, data = data, mapping = mapping, geom = "line", 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, arg = arg, ...))
    } else NULL) 
}
geom_lasagna <- function(mapping = list(), 
  data = NULL,  position = "identity", na.rm = FALSE, show.legend = NA, 
  inherit.aes = TRUE, arg = NULL, order_by = NULL, size = 4, ...) {
  default_mapping <- 
    aes(x = stat(.arg), y = stat(.id), colour = stat(.value))
  mapping <- structure(modifyList(mapping, default_mapping), class = "uneval")
  layer(
    stat = StatTfd, data = data, mapping = mapping, geom = "line",
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arg = arg, size = size, 
      order_by = order_by)
  ) 
}

set.seed(1221)
data = data.frame(col = sample(gl(5, 1)))
data$f = rgp(5)
data$fi = jiggle(data$f)
data$fb = tfb(data$f)

ggplot(data, aes(tfd = f, color = depth(f))) + geom_spaghetti() + 
  annotate("text", x = 1.05, y = drop(data$f[, 1]), label = 1:nrow(data))
ggplot(data, aes(tfd = f, shape = col, color = col)) + geom_meatballs()
ggplot(data, aes(tfd = f)) + geom_meatballs(spaghetti = FALSE) + 
  facet_wrap(~col)

# geom_lasagna is a hack of geom_line because geom_tile won't accept the 
# id-factor as a vertical axis -- adjust line width ("size") to get a proper 
# lasagna without gaps just like nonna used to make back in the old country:
ggplot(data, aes(tfd = fb, order = col)) + geom_lasagna(size = 8)
ggplot(data, aes(tfd = fb)) + geom_lasagna() + facet_wrap(~ col, scales = "free")

# use the order-aesthetic to define the vertical ordering of the functions 
# (low values - low positions)
ggplot(data, aes(tfd = f, order = -depth(f))) + geom_lasagna(size = 8)
# or use order_by to define an ordering computed directly 
# on each function's evaluations:
ggplot(data, aes(tfd = f)) + geom_lasagna(order_by = mean)
ggplot(data, aes(tfd = f)) + geom_lasagna(order_by = min)
ggplot(data, aes(tfd = f)) + geom_lasagna(order_by = function(f) f[1])
# last one same as this:
ggplot(data, aes(tfd = f, order = drop(f[, 0]))) + geom_lasagna()

#can even combine the two: 
ggplot(data, aes(tfd = f, order = col)) + geom_lasagna(order_by = function(f) f[1]) + 
  facet_wrap(col ~ ., scales = "free")



# look at GeomSf: https://github.com/tidyverse/ggplot2/blob/17b45f906c3a55d187915f4f3010836a804051ae/R/sf.R

   
   
  
