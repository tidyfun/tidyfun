## R CMD check results

0 errors | 0 warnings | 3 notes

## Test environments

* local: Linux Mint 22.1, R 4.5.2
  `R CMD build`
  `R CMD check --as-cran`
  `devtools::check(remote = TRUE, manual = TRUE)`
  `pkgdown::build_site()`
* R-hub: `rhub::rhub_doctor()` reports the repository is ready for
  `rhub::rhub_check()`
* win-builder: `devtools::check_win_devel()` submitted on 2026-04-24,
  results pending by email

## Resubmission

This resubmission addresses the CRAN warning from the new r-devel data check
for namespace references in packaged `.rda` files.

- Rebuilt `data/chf_df.rda` with the current `tf` constructors before saving.
- The packaged dataset no longer carries stale namespace references to
  `tidyfun` and `memoise`; both packaged datasets now only load `tf`, which is
  already a recursive strong dependency.
- Added the corresponding reconstruction step to `data-raw/chf_df.R` so future
  dataset rebuilds keep the serialized object aligned with the current `tf`
  package classes.

## Notes

- The local `--as-cran` run reports three environment-specific NOTES:
  no outbound network access for incoming/URL checks in the sandbox,
  `unable to verify current time`, and missing `tidy` for optional HTML
  validation.
- `pkgdown::build_site()` succeeds. The site build reports missing alt text for
  several plots in `x06_Registration.Rmd`; this affects site accessibility but
  is not a CRAN check failure.
- `devtools::check(remote = TRUE, manual = TRUE)` completed successfully after
  building the current package state, although its remote incoming-feasibility
  probe hit a Posit Package Manager `404` in this environment. The explicit
  `R CMD check --as-cran` tarball run above is the authoritative local check.

## Reverse dependencies

No reverse dependencies.
