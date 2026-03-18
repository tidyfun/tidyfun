# Documentation for tidyfun datasets

#' Congestive heart failure accelerometry data
#'
#' @description
#' Activity data from a study of congestive heart failure (CHF) patients. Data
#' were originally presented in Huang et al. (2019); these data are publicly
#' available, with download information in the paper.
#'
#' @format A tibble with 329 rows and 8 columns:
#' \describe{
#'   \item{id}{(numeric) Subject identifier.}
#'   \item{gender}{(character) `"Male"` or `"Female"`.}
#'   \item{age}{(numeric) Age in years.}
#'   \item{bmi}{(numeric) Body mass index.}
#'   \item{event_week}{(numeric) Week of cardiac event.}
#'   \item{event_type}{(character) Type of cardiac event.}
#'   \item{day}{(ordered factor) Day of the week (`Mon` < `Tue` < ... < `Sun`).}
#'   \item{activity}{(`tfd_reg`) Minute-by-minute activity counts over
#'     a 24-hour period (arg domain 1--1440).}
#' }
#'
#' @source
#' Data are from a study of physical activity in CHF patients conducted by
#' Huang et al. The original data are publicly available; see the paper for
#' download details.
#'
#' @references
#' Huang, L., Bai, J., Ivanescu, A., Harris, T., Maurer, M., Green, P., and
#' Zipunnikov, V. (2019). Multilevel Matrix-Variate Analysis and its
#' Application to Accelerometry-Measured Physical Activity in Clinical
#' Populations. *Journal of the American Statistical Association*, 114(526),
#' 553--564. \doi{10.1080/01621459.2018.1482750}
#'
#' @seealso [dti_df] for another example dataset,
#'   `vignette("x04_Visualization", package = "tidyfun")` for usage examples.
#' @keywords data
#' @family tidyfun datasets
#' @examples
#' chf_df
#'
#' library(ggplot2)
#' chf_df |>
#'   dplyr::filter(id %in% 1:5) |>
#'   gglasagna(activity, order_by = mean)
"chf_df"

#' Diffusion tensor imaging data
#'
#' @description
#' Fractional anisotropy (FA) tract profiles for the corpus callosum (`cca`)
#' and the right corticospinal tract (`rcst`) from a diffusion tensor imaging
#' (DTI) study of multiple sclerosis patients and healthy controls.
#'
#' The original data in [refund::DTI] include additional variables (`pasat`,
#' `Nscans`, `visit.time`) that were not carried over here.
#'
#' @format A tibble with 382 rows and 6 columns:
#' \describe{
#'   \item{id}{(numeric) Subject identifier.}
#'   \item{visit}{(integer) Visit number.}
#'   \item{sex}{(factor) `"male"` or `"female"`.}
#'   \item{case}{(factor) `"control"` or `"MS"` (multiple sclerosis status).}
#'   \item{cca}{(`tfd_irreg`) FA tract profiles for the corpus callosum
#'     (up to 93 evaluation points, domain 0--1).}
#'   \item{rcst}{(`tfd_irreg`) FA tract profiles for the right corticospinal
#'     tract (up to 55 evaluation points, domain 0--1).}
#' }
#'
#' @details
#' If you use this data as an example in written work, please include the
#' following acknowledgment: *"The MRI/DTI data were collected at Johns Hopkins
#' University and the Kennedy-Krieger Institute."*
#'
#' @source
#' Data are from the Johns Hopkins University and the Kennedy-Krieger Institute.
#' Also available in a different format as [refund::DTI].
#'
#' @references
#' Goldsmith, J., Bobb, J., Crainiceanu, C., Caffo, B., and Reich, D. (2011).
#' Penalized Functional Regression. *Journal of Computational and Graphical
#' Statistics*, 20(4), 830--851. \doi{10.1198/jcgs.2010.10007}
#'
#' Goldsmith, J., Crainiceanu, C., Caffo, B., and Reich, D. (2012).
#' Longitudinal Penalized Functional Regression for Cognitive Outcomes on
#' Neuronal Tract Measurements. *Journal of the Royal Statistical Society:
#' Series C*, 61(3), 453--469. \doi{10.1111/j.1467-9876.2011.01031.x}
#'
#' @seealso [chf_df] for another example dataset,
#'   `vignette("x04_Visualization", package = "tidyfun")` for usage examples.
#' @keywords data
#' @family tidyfun datasets
#' @examples
#' dti_df
#'
#' library(ggplot2)
#' dti_df |>
#'   dplyr::filter(visit == 1) |>
#'   tf_ggplot(aes(tf = cca, color = case)) +
#'   geom_line(alpha = 0.3)
"dti_df"
