# Visual Testing for tf_ggplot

This directory contains comprehensive visual testing tools for the tf_ggplot functionality. These tools help ensure that tf_ggplot produces sensible, correct, and visually appealing plots.

## Overview

The visual testing system provides three levels of testing:

1. **Unit Tests with Visual Validation** - Standard testthat tests that also save plots
2. **Interactive Development Testing** - Quick tests for development workflow  
3. **Comprehensive Visual Reports** - Automated HTML reports with all test plots

## Files

### Test Files
- `test-tf-ggplot-visual.R` - Main visual tests with plot generation
- `test-tf-ggplot-basic.R` - Basic functionality tests
- `test-tf-ggplot-advanced.R` - Advanced feature tests
- `test-tf-ggplot-aesthetic-equivalence.R` - Aesthetic specification equivalence tests
- `helper-tf-ggplot.R` - Test utilities and helper functions

### Development Tools
- `visual-test-runner.R` - Comprehensive test runner with HTML report generation
- `quick-visual-check.R` - Interactive development testing tool
- `README-visual-testing.md` - This documentation

## Usage

### 1. Quick Development Testing

For rapid development and debugging:

```r
# Load the quick testing tools
source("tests/quick-visual-check.R")

# Run interactive test menu
run_interactive_tests()

# Or run specific tests
test_basic_line()          # Test basic line plots
test_ribbon_plot()         # Test confidence bands
test_faceting()           # Test faceted plots
test_multiple_aesthetics() # Test complex aesthetic mappings
```

### 2. Comprehensive Visual Testing

For thorough testing with HTML reports:

```r
# Load the test runner
source("tests/visual-test-runner.R")

# Run all tests and create HTML report
run_tf_ggplot_visual_tests()

# Options:
run_tf_ggplot_visual_tests(
  interactive_display = TRUE,    # Show plots as they're created
  create_report = TRUE,          # Generate HTML report (default)
  open_report = TRUE             # Auto-open report in browser (default)
)

# Just create report from existing plots
create_visual_report()
```

### 3. Standard Unit Testing

Run through testthat:

```r
# Run specific visual test file
testthat::test_file("tests/testthat/test-tf-ggplot-visual.R")

# Run all tf_ggplot tests
testthat::test_dir("tests/testthat", filter = "tf-ggplot")
```

## Output Structure

The visual testing system creates organized output:

```
tests/
├── visual-output/
│   ├── plots/                    # Individual plot PNG files
│   │   ├── 01_basic_line_constructor.png
│   │   ├── 02_basic_line_geom.png
│   │   ├── ...
│   │   └── 17_wide_range.png
│   ├── reports/                  # HTML reports
│   │   └── visual_test_report.html
│   └── comparisons/              # Side-by-side comparisons
└── test-plots/                   # Basic test output (legacy)
```

## Test Categories

### Basic Functionality
- Constructor with different aesthetic specifications
- Simple line plots with grouping
- Basic geom integration

### Advanced Features
- Multiple tf aesthetics (tf_x, tf_y)
- Ribbon plots with confidence bands
- Faceting with tf data
- Complex aesthetic mappings
- Different geom types (line, point, step, area)

### Comparisons
- tf_ggplot vs current geom_spaghetti
- Constructor vs geom-level aesthetic specification
- Different evaluation grids

### Edge Cases
- Single functions
- Very sparse data (few evaluation points)
- Many functions (visual clutter)
- Wide evaluation ranges
- Irregular tf objects
- NA/empty functions

## Visual Validation Checklist

When reviewing plots, check for:

### ✅ Correctness
- [ ] Functions are plotted with correct x/y values
- [ ] Grouping is preserved (separate lines per function)
- [ ] Colors/aesthetics are applied consistently within groups
- [ ] Faceting splits data correctly
- [ ] Evaluation grids are respected

### ✅ Visual Quality
- [ ] Lines are smooth and continuous
- [ ] Points are clearly visible when used
- [ ] Ribbons show appropriate confidence regions
- [ ] Colors are distinguishable
- [ ] Transparency/alpha works correctly
- [ ] Text and labels are readable

### ✅ Equivalence
- [ ] tf_ggplot plots match geom_spaghetti reference plots
- [ ] Constructor aesthetics ≡ geom-level aesthetics
- [ ] Different geom types produce expected shapes
- [ ] Custom evaluation grids work correctly

### ✅ Edge Cases
- [ ] Single functions render without errors
- [ ] Sparse data shows appropriate point gaps
- [ ] Many functions are readable with transparency
- [ ] Wide ranges don't cause axis problems
- [ ] Irregular data handles missing points gracefully

## Development Workflow

### 1. Initial Implementation
```r
# Quick check during development
source("tests/quick-visual-check.R")
test_basic_line()  # Will show current geom_spaghetti for reference
```

### 2. Feature Testing
```r
# Test specific features as you implement them
test_ribbon_plot()         # For ribbon geom support
test_multiple_aesthetics() # For complex aesthetic handling
```

### 3. Comprehensive Validation
```r
# Full test suite with report
source("tests/visual-test-runner.R")
run_tf_ggplot_visual_tests()
# Review HTML report for all plots
```

### 4. Regression Testing
```r
# Standard unit tests
testthat::test_dir("tests/testthat")
```

## Customization

### Adding New Visual Tests

Add tests to `test-tf-ggplot-visual.R`:

```r
test_that("my new feature looks correct", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()
  
  # Create test data
  data <- create_test_tf_data(...)
  
  # Create plot
  p <- tf_ggplot(data, aes(...)) + geom_xxx(...)
  
  # Save for visual inspection
  save_test_plot(p, "my_new_feature")
  
  # Basic validation
  expect_s3_class(p, "ggplot")
  # ... other checks
})
```

### Custom Plot Categories

Modify `save_visual_test_plot()` in `visual-test-runner.R` to add new categories.

### Integration with CI/CD

For automated testing, disable interactive features:

```r
run_tf_ggplot_visual_tests(
  interactive_display = FALSE,
  create_report = TRUE,
  open_report = FALSE
)
```

## Troubleshooting

### Common Issues

1. **Plots not saving**: Check write permissions in tests/ directory
2. **HTML report not opening**: Set `open_report = FALSE` and open manually
3. **Missing tf functions**: Load tf/tidyfun packages first
4. **Memory issues**: Test with smaller datasets during development

### Performance

For large test suites:
- Use coarser evaluation grids during development
- Limit number of functions in test data
- Use `ggsave()` with lower DPI for faster saving

### Platform-Specific Notes

- **Windows**: HTML reports open with default browser
- **macOS/Linux**: Uses `open`/`xdg-open` commands
- **Server/headless**: Set `interactive_display = FALSE`

## Contributing

When adding new tf_ggplot features:

1. Add corresponding visual tests
2. Update this README if new test categories are added
3. Ensure visual tests pass on your development platform
4. Include example plots in PR descriptions when possible

The visual testing system is designed to grow with the tf_ggplot functionality and provide confidence that the implementation produces correct, beautiful functional data visualizations.