# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build/Test Commands

- Full package check: `Rscript -e "devtools::check()"`
- In R session:
  - Re-load package: `devtools::load_all()`
  - Update documentation: `devtools::document()`
  - Run all tests: `devtools::test()`
  - Run single test: `testthat::test_file("tests/testthat/test-filename.R")`
  - Code coverage: `covr::package_coverage()`
- In the terminal, wrap the commands above like this: `Rscript -e "devtools::load_all(); <COMMAND>"`

Always reload the package with `devtools::load_all()` before testing changes.

## Code Style Guidelines

- Function naming: Use snake_case with `tf_` prefix for exported functions
- Method implementation: Use `ClassName.method` pattern for S3 methods
- Documentation: Use roxygen2 with markdown formatting (@param, @return)
- Imports: Declare dependencies in DESCRIPTION file; avoid adding dependencies to additional packages.
- Error handling: Use cli::cli_abort() or cli::cli_warning() for errors and warnings, or use checkmate-assertions and the custom assertions from assertions.R
- Object creation: Follow vctrs framework for S3 classes
- Testing: Write unit tests with testthat v3 (expect_equal, expect_error)
- PRs: Make pull requests against the 'dev' branch

## Additional Directories

- `home/fabians/fda/tidyfun-pkgs/tf/` contains the code and documentation for the underlying `tf`-classes and methods, 
  always read/refer to those since they are the basis for this package.

