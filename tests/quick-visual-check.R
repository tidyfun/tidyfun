# Quick Visual Check Script for tf_ggplot Development
# Run this to quickly test and view tf_ggplot functionality during development

# Load required libraries
library(ggplot2)

# Source tf_ggplot functions (adjust path as needed)
# source("R/tf-ggplot.R")  # Uncomment when implemented

# Quick test data generator
quick_test_data <- function(n_funcs = 3, pattern = "random") {
  set.seed(123)
  data <- data.frame(
    id = seq_len(n_funcs),
    group = factor(sample(c("A", "B"), n_funcs, replace = TRUE))
  )

  arg_vals <- seq(0, 2 * pi, length.out = 51)

  if (pattern == "random") {
    data$func <- tf_rgp(n_funcs, arg = arg_vals)
  } else if (pattern == "trig") {
    func_vals <- list(
      sin(arg_vals),
      cos(arg_vals),
      sin(2 * arg_vals)
    )[seq_len(n_funcs)]
    data$func <- tfd(func_vals, arg = arg_vals)
  } else if (pattern == "poly") {
    func_vals <- list(
      arg_vals,
      arg_vals^2,
      arg_vals^3
    )[seq_len(n_funcs)]
    data$func <- tfd(func_vals, arg = arg_vals)
  }

  return(data)
}

# Quick visual tests - run these individually during development
test_basic_line <- function() {
  cat("Testing: Basic line plot\n")
  data <- quick_test_data(3, "trig")

  # Test current approach
  p1 <- ggplot(data, aes(y = func, color = group)) +
    geom_spaghetti() +
    labs(title = "Current: geom_spaghetti") +
    theme_minimal()

  # Test new approach (will fail until implemented)
  tryCatch(
    {
      p2 <- tf_ggplot(data, aes(tf = func, color = group)) +
        geom_line() +
        labs(title = "New: tf_ggplot + geom_line") +
        theme_minimal()

      print(p1)
      Sys.sleep(2) # Brief pause between plots
      print(p2)

      cat("✓ Basic line plot test passed\n")
      return(list(current = p1, new = p2))
    },
    error = function(e) {
      cat("✗ tf_ggplot not yet implemented:", e$message, "\n")
      print(p1)
      cat("Showing current geom_spaghetti for reference\n")
      return(list(current = p1, new = NULL))
    }
  )
}

test_ribbon_plot <- function() {
  cat("Testing: Ribbon/confidence band plot\n")

  # Create confidence band data
  set.seed(456)
  data <- data.frame(method = c("A", "B"))
  arg_vals <- seq(0, 5, length.out = 26)

  mean_vals1 <- 2 * exp(-arg_vals / 2) * cos(arg_vals)
  mean_vals2 <- 1.5 * exp(-arg_vals / 3) * sin(arg_vals * 1.2)

  sd1 <- rep(0.3, 26)
  sd2 <- rep(0.25, 26)

  data$mean_func <- tfd(list(mean_vals1, mean_vals2), arg = arg_vals)
  data$lower_func <- tfd(
    list(mean_vals1 - 1.96 * sd1, mean_vals2 - 1.96 * sd2),
    arg = arg_vals
  )
  data$upper_func <- tfd(
    list(mean_vals1 + 1.96 * sd1, mean_vals2 + 1.96 * sd2),
    arg = arg_vals
  )

  tryCatch(
    {
      p <- tf_ggplot(data, aes(color = method, fill = method)) +
        geom_ribbon(
          aes(tf_ymin = lower_func, tf_ymax = upper_func),
          alpha = 0.3
        ) +
        geom_line(aes(tf = mean_func), size = 1) +
        labs(title = "tf_ggplot: Confidence Bands") +
        theme_minimal()

      print(p)
      cat("✓ Ribbon plot test passed\n")
      return(p)
    },
    error = function(e) {
      cat("✗ tf_ggplot ribbon test failed:", e$message, "\n")

      # Show what it should look like with manual transformation
      cat("This is what the ribbon plot should look like...\n")

      # Manual transformation for reference
      data_long <- tf_unnest(data, mean_func)
      data_bands <- tf_unnest(data, c(lower_func, upper_func))

      p_ref <- ggplot() +
        geom_ribbon(
          data = data_bands,
          aes(x = arg, ymin = lower_func, ymax = upper_func, fill = method),
          alpha = 0.3
        ) +
        geom_line(
          data = data_long,
          aes(x = arg, y = mean_func, color = method),
          size = 1
        ) +
        labs(title = "Reference: Manual transformation", x = "x", y = "f(x)") +
        theme_minimal()

      print(p_ref)
      return(p_ref)
    }
  )
}

test_faceting <- function() {
  cat("Testing: Faceted plot\n")

  data <- expand.grid(
    subject = 1:4,
    condition = c("Control", "Treatment")
  )
  data$group <- factor(rep(c("A", "B"), 4))

  # Create different patterns by condition
  set.seed(789)
  arg_vals <- seq(0, 10, length.out = 21)
  func_vals <- map(seq_len(nrow(data)), function(i) {
    base <- sin(arg_vals * 0.5) + rnorm(21, 0, 0.1)
    if (data$condition[i] == "Treatment") {
      base <- base * 1.5 + 0.5
    }
    base
  })

  data$func <- tfd(func_vals, arg = arg_vals)

  tryCatch(
    {
      p <- tf_ggplot(data, aes(tf = func, color = group)) +
        geom_line(alpha = 0.7) +
        facet_wrap(~condition) +
        labs(title = "tf_ggplot: Faceted Plot") +
        theme_minimal()

      print(p)
      cat("✓ Faceting test passed\n")
      return(p)
    },
    error = function(e) {
      cat("✗ tf_ggplot faceting test failed:", e$message, "\n")
      return(NULL)
    }
  )
}

test_multiple_aesthetics <- function() {
  cat("Testing: Multiple aesthetics\n")

  data <- expand.grid(
    method = c("A", "B", "C"),
    difficulty = c("Easy", "Hard")
  )
  data$performance <- runif(nrow(data))

  set.seed(101112)
  arg_vals <- seq(0, 10, length.out = 26)
  func_vals <- map(seq_len(nrow(data)), function(i) {
    base <- data$performance[i] * (1 - exp(-arg_vals / 3))
    if (data$difficulty[i] == "Hard") base <- base * 0.7
    base + rnorm(26, 0, 0.02)
  })

  data$learning_curve <- tfd(func_vals, arg = arg_vals)

  tryCatch(
    {
      p <- tf_ggplot(
        data,
        aes(
          tf = learning_curve,
          color = method,
          linetype = difficulty,
          alpha = performance
        )
      ) +
        geom_line(size = 1) +
        scale_alpha_continuous(range = c(0.4, 1.0)) +
        labs(title = "tf_ggplot: Multiple Aesthetics") +
        theme_minimal()

      print(p)
      cat("✓ Multiple aesthetics test passed\n")
      return(p)
    },
    error = function(e) {
      cat("✗ tf_ggplot multiple aesthetics test failed:", e$message, "\n")
      return(NULL)
    }
  )
}

# Interactive test runner
run_interactive_tests <- function() {
  cat("\n=== tf_ggplot Interactive Visual Tests ===\n\n")

  tests <- list(
    "1" = list(name = "Basic Line Plot", func = test_basic_line),
    "2" = list(name = "Ribbon/Confidence Bands", func = test_ribbon_plot),
    "3" = list(name = "Faceted Plot", func = test_faceting),
    "4" = list(name = "Multiple Aesthetics", func = test_multiple_aesthetics),
    "a" = list(name = "Run All Tests", func = function() {
      test_basic_line()
      test_ribbon_plot()
      test_faceting()
      test_multiple_aesthetics()
    })
  )

  repeat {
    cat("\nAvailable tests:\n")
    for (key in names(tests)) {
      cat(sprintf("  %s) %s\n", key, tests[[key]]$name))
    }
    cat("  q) Quit\n\n")

    choice <- readline("Choose a test (1-4, a, q): ")

    if (choice == "q") {
      cat("Goodbye!\n")
      break
    }

    if (choice %in% names(tests)) {
      cat("\n", rep("=", 50), "\n")
      result <- tests[[choice]]$func()
      cat(rep("=", 50), "\n")

      if (!is.null(result)) {
        cat("Press Enter to continue...")
        readline()
      }
    } else {
      cat("Invalid choice. Please try again.\n")
    }
  }
}

# Development helpers
check_tf_functions <- function() {
  cat("Checking for required tf functions...\n")

  required_funcs <- c("tf_rgp", "tfd", "tf_unnest", "tf_evaluations", "tf_arg")
  available <- sapply(required_funcs, exists)

  for (i in seq_along(required_funcs)) {
    status <- if (available[i]) "✓" else "✗"
    cat(sprintf("%s %s\n", status, required_funcs[i]))
  }

  if (all(available)) {
    cat("\n✓ All required tf functions are available.\n")
  } else {
    cat("\n✗ Some tf functions are missing. Load the tf/tidyfun packages.\n")
  }

  return(all(available))
}

check_ggplot_functions <- function() {
  cat("Checking for ggplot2 functions...\n")

  required_funcs <- c("ggplot", "aes", "geom_line", "geom_point", "geom_ribbon")
  available <- sapply(required_funcs, exists)

  for (i in seq_along(required_funcs)) {
    status <- if (available[i]) "✓" else "✗"
    cat(sprintf("%s %s\n", status, required_funcs[i]))
  }

  return(all(available))
}

# Print startup message
cat("\n=== tf_ggplot Quick Visual Check ===\n")
cat("Available functions:\n")
cat("  test_basic_line()         # Test basic line plots\n")
cat("  test_ribbon_plot()        # Test ribbon/confidence bands\n")
cat("  test_faceting()           # Test faceted plots\n")
cat("  test_multiple_aesthetics() # Test complex aesthetics\n")
cat("  run_interactive_tests()   # Interactive test menu\n")
cat("  check_tf_functions()      # Check tf package availability\n")
cat("  check_ggplot_functions()  # Check ggplot2 availability\n\n")

# Run basic checks
if (interactive()) {
  check_tf_functions()
  check_ggplot_functions()

  cat("\nReady for testing! Try run_interactive_tests() to start.\n")
}
