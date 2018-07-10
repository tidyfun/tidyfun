# cf. https://github.com/tidyverse/ggplot2/blob/69dfc4bc62828ecc3d1cda3dea4d0a664c4ea12c/R/sf.R
is.finite.feval <- function(x) rep(TRUE, length(x))
scale_type.fvector <- function(x) "continuous"
library(magrittr)
library(tidyverse)

StatSpaghetti <- ggproto("StatSpaghetti", Stat,
  compute_layer = function(data, scales, ..., argvals) {
    fdata <- evaluate(data, argvals, fun) %>%
      select(-group) %>% 
      unnest(.id = "group") %>% 
      rename(x = argvals, y = data)
    fdata
  },
  required_aes = c("fun")
)
stat_spaghetti <- function(mapping = NULL, data = NULL,
  position = "identity", na.rm = FALSE, show.legend = NA, 
  inherit.aes = TRUE, ...) {
  layer(
    stat = StatSpaghetti, data = data, mapping = mapping, geom = "line", 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

StatLasagna <- ggproto("StatLasagna", Stat,
  compute_layer = function(data, scales, ..., argvals) {
    fdata <- evaluate(data, argvals = argvals, fun) %>%
      select(-group) %>% 
      unnest(.id = "y") %>% mutate(y = as.numeric(y)) %>% 
      rename(x = argvals, fill = data)
    fdata
  },
  required_aes = c("fun")
)
stat_lasagna <- function(mapping = NULL, data = NULL,
  position = "identity", na.rm = FALSE, show.legend = NA, 
  inherit.aes = TRUE, ...) {
  layer(
    stat = StatLasagna, data = data, mapping = mapping, geom = "tile", 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


data = data.frame(col = gl(5, 10)) 
data$f = rgp(50)
ggplot(data, aes(fun = f, colour = col)) + stat_spaghetti(alpha = .5)
p <- ggplot(data, aes(fun = f)) + stat_lasagna()

p = ggplot(data, aes(fun = f)) + stat_spaghetti()



# look at GeomSf!

   
   
  
