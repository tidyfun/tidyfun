data <- refund::DTI[1:5, ]
d1 <- data.frame(data$cca)
d2 <- data.frame(id = data$ID, data$cca)
cca <- tfd(data$cca)

test_that("tf_gather works", {
  expect_s3_class(tf_gather(d1)$cca |> suppressMessages(), "tfd")
  expect_message(tf_gather(d1)$cca, "cca")
  expect_identical(tf_gather(d1)$cca |> suppressMessages(), cca)
  expect_equal(
    tf_gather(d1)$cca |> suppressMessages(),
    tf_gather(data[, 1:8], cca)$cca |> suppressMessages(),
    ignore_attr = TRUE
  )

  expect_identical(tf_gather(d2, -1)$cca |> suppressMessages(), cca)
  expect_identical(tf_gather(d2, -1)$id |> suppressMessages(), d2$id)
  expect_identical(
    tf_gather(d2, -1, key = "nuhnuh")$nuhnuh |>
      suppressMessages(),
    tf_gather(d2, -1)$cca |> suppressMessages()
  )

  expect_identical(
    tf_gather(d2, -id)$cca |> suppressMessages(),
    tf_gather(d2, -1)$cca |> suppressMessages()
  )
  expect_identical(
    tf_gather(d2, starts_with("cca"))$cca |> suppressMessages(),
    tf_gather(d2, -1)$cca |> suppressMessages()
  )

  expect_identical(
    attr(tf_gather(d1, evaluator = tf_approx_spline)$cca, "evaluator_name") |>
      suppressMessages(),
    "tf_approx_spline"
  )

  expect_named(tf_gather(d1) |> suppressMessages(), "cca")
})

test_that("tf_spread works", {
  d <- tibble(g = 1:3, f = tf_rgp(3, 11L))
  expect_equal(
    tf_spread(d, f, sep = NULL)[, -1],
    as.data.frame(as.matrix(d$f)),
    ignore_attr = TRUE
  )
  expect_identical(tf_spread(d, f), tf_spread(d, -g))
  expect_identical(tf_spread(d, f), tf_spread(d))
  expect_warning(
    tf_spread(d, f, arg = seq(0, 1, length.out = 20), sep = NULL),
    "interpolate = FALSE"
  )
  expect_equal(
    suppressWarnings(
      tf_spread(d, f, arg = seq(0, 1, length.out = 20), sep = NULL)[, -1]
    ),
    suppressWarnings(
      as.data.frame(d$f[, seq(0, 1, length.out = 20), interpolate = FALSE])
    ),
    ignore_attr = TRUE
  )
  d$fb <- tfb(tf_rgp(3, 11L), verbose = FALSE) |> suppressWarnings()
  expect_error(tf_spread(d), "More than one")
  expect_equal(
    tf_spread(d, fb, sep = NULL)[, -(1:2)] |> suppressMessages(),
    as.data.frame(as.matrix(d$fb)),
    ignore_attr = TRUE
  )
  set.seed(1312)
  d$fi <- tf_jiggle(tf_rgp(3, 11L))
  tf_spread(d, fi) |>
    expect_warning("no explicit `arg` for irregular") |>
    suppressWarnings()
  tf_spread(d, fi) |>
    expect_warning("interpolate = FALSE") |>
    suppressWarnings()
  expect_true(suppressWarnings(ncol(tf_spread(d, fi)) == 36))
  expect_equal(
    tf_spread(
      d,
      fi,
      arg = seq(0, 1, length.out = 20),
      sep = NULL,
      interpolate = TRUE
    )[, -(1:3)],
    as.data.frame(
      as.matrix(d$fi, arg = seq(0, 1, length.out = 20), interpolate = TRUE)
    ),
    ignore_attr = TRUE
  )
})

test_that("tf_nest works", {
  f1 <- tf_rgp(3, 11L)
  f2 <- tf_rgp(3, 11L)
  data <- dplyr::inner_join(tf_unnest(f1), tf_unnest(f2), by = c("id", "arg"))
  expect_equal(tf_nest(data)$value.x, f1, ignore_attr = TRUE)
  expect_equal(tf_nest(data)$value.y, f2, ignore_attr = TRUE)
  expect_named(tf_nest(data, value.x:value.y), names(tf_nest(data)))
  expect_named(tf_nest(data, -(1:2)), names(tf_nest(data)))

  g <- rnorm(3)
  data <- bind_cols(data, g = rep(g, e = tf_count(f1)))
  expect_error(tf_nest(data |> group_by(g)), "grouped_df")
  expect_identical(tf_nest(data, value.x:value.y)$g, g)
  data <- bind_cols(data, f = rep(rnorm(nrow(data))))
  expect_error(tf_nest(data, value.x:value.y), "Can't nest")
})

# weird scoping problem going on -- fixed by assigning
# data to tf_evaluate as evaluated object instead of quoted for now,
# otherwise tfdata is not found inside test environments (related to testthat/#266?)
test_that("tf_unnest works", {
  set.seed(121211)
  f1 <- tf_rgp(3, 11L)
  f2 <- tf_rgp(3, 11L)
  data <- dplyr::inner_join(tf_unnest(f1), tf_unnest(f2), by = c("id", "arg"))
  tfdata <- tf_nest(data)
  expect_identical(NCOL(tf_unnest(tfdata, cols = c(value.x, value.y))), 5L)
  expect_equal(
    as.matrix(tf_unnest(tfdata, cols = c(value.x, value.y))[-c(1, 4)]),
    as.matrix(data[, 2:4]),
    ignore_attr = TRUE
  )
  expect_s3_class(tf_unnest(tfdata, value.x)$value.y, "tfd")
})

# tidyfun#109
test_that("tf_nest / tf_unnest work with numeric id-variables", {
  d <- tf_rgp(10) |> tf_unnest()
  d$id <- as.numeric(d$id) * 10
  nested <- tf_nest(d)
  unnested <- tf_unnest(nested, cols = value)
  expect_equal(d, unnested, ignore_attr = TRUE)
})

# Multivariate (tf_mv) -------------------------------------------------------

test_that("tf_unnest.tf_mv returns wide (id, arg, <components>)", {
  set.seed(1)
  mv <- tfd_mv(list(x = tf_rgp(3, 11L), y = tf_rgp(3, 11L)))
  out <- tf_unnest(mv)
  expect_named(out, c("id", "arg", "x", "y"))
  expect_s3_class(out$id, "ordered")
  expect_equal(nrow(out), 3 * 11)
  # values match the underlying components
  expect_equal(
    sort(out$x),
    sort(as.numeric(unlist(tf_evaluations(tf_component(mv, 1)))))
  )
})

test_that("tf_unnest.tf_mv matches tf::as.data.frame on the native grid", {
  set.seed(2)
  mv <- tfd_mv(list(a = tf_rgp(4, 11L), b = tf_rgp(4, 11L)))
  ours <- tf_unnest(mv)
  ref <- as.data.frame(mv, unnest = TRUE)
  expect_equal(sort(ours$a), sort(ref$a))
  expect_equal(sort(ours$b), sort(ref$b))
})

test_that("tf_unnest.tf_mv honours a custom arg grid", {
  set.seed(3)
  mv <- tfd_mv(list(x = tf_rgp(3, 11L), y = tf_rgp(3, 11L)))
  out <- tf_unnest(mv, arg = seq(0, 1, length.out = 5))
  expect_equal(nrow(out), 3 * 5)
  expect_setequal(out$arg, seq(0, 1, length.out = 5))
})

test_that("tf_unnest.tf_mv full-outer-joins misaligned component grids", {
  set.seed(4)
  cx <- tfd(matrix(rnorm(2 * 6), 2), arg = seq(0, 0.6, length.out = 6))
  cy <- tfd(matrix(rnorm(2 * 6), 2), arg = seq(0.4, 1.0, length.out = 6))
  mv <- tfd_mv(list(x = cx, y = cy))
  out <- tf_unnest(mv)
  # union grid -> some rows have NA in exactly one component
  expect_true(any(is.na(out$x) & !is.na(out$y)))
  expect_true(any(is.na(out$y) & !is.na(out$x)))
})
