## R CMD check results

0 errors | 0 warnings | 1 note

The NOTE is the standard "New submission" note plus:

- "geoms" flagged as possibly misspelled (see below)
- DOI URL https://doi.org/10.1214/aos/1176348794 returns 404 intermittently;
  this is a valid DOI for Kneip & Gasser (1992, Annals of Statistics) and
  resolves correctly in a browser
- Tarball size ~8.7 MB (vignettes with figures account for most of the size)

## Test environments

* local: Linux Mint 22.1, R 4.5.2
* GitHub Actions: ubuntu-latest (R-release, R-devel), windows-latest
  (R-release), macos-latest (R-release)

## First submission

This is the first CRAN submission of tidyfun.

## Reverse dependencies

No reverse dependencies (first submission).

## DESCRIPTION spelling

"geoms" is an established term in the ggplot2 ecosystem (short for
"geometric objects") and is used intentionally.
