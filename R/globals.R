# clean up globals for R CMD check
utils::globalVariables(c(
  ".", "..fill", "..label", "..order", "..order_by_value",
  "..row", "..x", "..y", "V1", "V2", "id_num", "tick_hi", "tick_lo",
  "tick_pos", "ticks", "value", "xmax"
))
