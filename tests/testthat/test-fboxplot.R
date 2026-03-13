make_fboxplot_data <- function() {
  arg <- seq(0, 1, length.out = 21)
  curves <- rbind(
    1.0 + 0.10 * sin(2 * pi * arg),
    1.0 + 0.12 * sin(2 * pi * arg + 0.1),
    0.98 + 0.09 * sin(2 * pi * arg - 0.1),
    1.55 + 0.10 * sin(2 * pi * arg),
    1.2 + 0.08 * cos(2 * pi * arg),
    1.22 + 0.09 * cos(2 * pi * arg + 0.1),
    1.18 + 0.08 * cos(2 * pi * arg - 0.1),
    1.75 + 0.10 * cos(2 * pi * arg)
  )

  data.frame(
    id = seq_len(nrow(curves)),
    grp = factor(rep(c("A", "B"), each = 4)),
    facet_grp = factor(rep(c("left", "right"), times = 4))
  ) |>
    dplyr::mutate(func = tfd(curves, arg = arg))
}

test_that("geom_fboxplot builds on tf_ggplot", {
  data <- make_fboxplot_data()

  p <- tf_ggplot(data, aes(tf = func)) + geom_fboxplot()
  built <- suppressWarnings(ggplot_build(p))
  plot_data <- built$data[[1]]

  expect_true(all(
    c(".fbox_component", ".fbox_stat_group") %in% names(plot_data)
  ))
  expect_true(all(
    c("box", "median", "whisker_lower", "whisker_upper") %in%
      unique(plot_data$.fbox_component)
  ))
})

test_that("geom_fboxplot sets tf axis labels like standard tf geoms", {
  data <- make_fboxplot_data()

  p <- tf_ggplot(data, aes(tf = func)) + geom_fboxplot()
  built <- suppressWarnings(ggplot_build(p))

  expect_equal(built$plot$labels$y, "func")
  expect_equal(built$plot$labels$x, "func.arg")
})

test_that("geom_fboxplot swaps default axis labels for horizontal orientation", {
  data <- make_fboxplot_data()

  p <- tf_ggplot(data, aes(tf = func)) + geom_fboxplot(orientation = "y")
  built <- suppressWarnings(ggplot_build(p))

  expect_equal(built$plot$labels$x, "func")
  expect_equal(built$plot$labels$y, "func.arg")
})

test_that("geom_fboxplot groups within a panel by fill and colour", {
  data <- make_fboxplot_data()

  p <- tf_ggplot(data, aes(tf = func, fill = grp, colour = grp)) +
    geom_fboxplot()
  built <- suppressWarnings(ggplot_build(p))

  expect_equal(length(unique(built$data[[1]]$.fbox_stat_group)), 2)
})

test_that("geom_fboxplot uses mapped colour for ribbons when fill is absent", {
  data <- make_fboxplot_data()

  p <- tf_ggplot(data, aes(tf = func, colour = grp)) + geom_fboxplot()
  panel_grob <- ggplotGrob(p)$grobs[[which(
    ggplotGrob(p)$layout$name == "panel"
  )]]
  layer_grob <- panel_grob$children[[3]]
  polygon_idx <- grepl("^geom_polygon", names(layer_grob$children))
  fills <- vapply(
    layer_grob$children[polygon_idx],
    \(g) g$gp$fill,
    character(1)
  )

  expect_equal(length(unique(fills)), 2)
})

test_that("geom_fboxplot is compatible with facetting", {
  data <- make_fboxplot_data()

  p <- tf_ggplot(data, aes(tf = func, fill = grp, colour = grp)) +
    geom_fboxplot() +
    facet_wrap(~facet_grp)
  built <- suppressWarnings(ggplot_build(p))

  expect_equal(as.character(sort(unique(built$data[[1]]$PANEL))), c("1", "2"))
})

test_that("geom_fboxplot is compatible with scaled axes", {
  data <- make_fboxplot_data()

  p <- tf_ggplot(data, aes(tf = func)) +
    geom_fboxplot() +
    scale_y_log10()
  built <- suppressWarnings(ggplot_build(p))
  numeric_cols <- intersect(
    c("x", "y", "ymin", "ymax", "xmin", "xmax"),
    names(built$data[[1]])
  )

  expect_true(length(numeric_cols) > 0)
  expect_true(all(vapply(
    built$data[[1]][numeric_cols],
    \(x) all(is.finite(x[!is.na(x)])),
    logical(1)
  )))
})

test_that("geom_fboxplot stat output is retransformed by y scales", {
  data <- make_fboxplot_data()
  data <- dplyr::mutate(data, pos_func = exp(func) + 100)

  p <- tf_ggplot(data, aes(tf = pos_func, fill = grp)) +
    geom_fboxplot() +
    scale_y_log10()
  built <- suppressWarnings(ggplot_build(p))
  numeric_cols <- intersect(
    c("y", "ymin", "ymax", "xmin", "xmax"),
    names(built$data[[1]])
  )
  vals <- unlist(built$data[[1]][numeric_cols])
  vals <- vals[is.finite(vals)]

  expect_true(max(vals) < 3)
  expect_true(min(vals) > 1)
})

test_that("geom_fboxplot supports flipped orientation", {
  data <- make_fboxplot_data()

  p <- tf_ggplot(data, aes(tf = func)) +
    geom_fboxplot(orientation = "y")
  built <- suppressWarnings(ggplot_build(p))
  plot_data <- built$data[[1]]

  expect_true(all(c("x", "y") %in% names(plot_data)))
  expect_true(all(
    c("xmin", "xmax") %in%
      names(plot_data[plot_data$.fbox_component == "box", ])
  ))
})

test_that("geom_fboxplot accepts custom depth and fence functions", {
  data <- make_fboxplot_data()

  depth_fn <- function(x, arg = NULL) {
    rev(seq_along(x))
  }
  fence_fn <- function(
    tf_vec,
    arg,
    depth,
    median,
    central_lower,
    central_upper,
    coef,
    central
  ) {
    list(
      lower = central_lower - 0.01,
      upper = central_upper + 0.01,
      outliers = c(rep(FALSE, length(tf_vec) - 1), TRUE)
    )
  }

  p <- tf_ggplot(data, aes(tf = func)) +
    geom_fboxplot(depth_fn = depth_fn, fence_fn = fence_fn)
  built <- suppressWarnings(ggplot_build(p))

  expect_true(any(built$data[[1]]$.fbox_component == "outlier"))
})
