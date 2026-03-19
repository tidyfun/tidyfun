# Package index

## tidyfun workflows

User-facing wrappers for working with functional data inside data frames

- [`tidyfun`](https://tidyfun.github.io/tidyfun/reference/tidyfun-package.md)
  [`tidyfun-package`](https://tidyfun.github.io/tidyfun/reference/tidyfun-package.md)
  : tidyfun: Tidy Functional Data Wrangling and Visualization

- [`tf_gather()`](https://tidyfun.github.io/tidyfun/reference/tf_gather.md)
  :

  Gather all columns representing functional measurements into a
  `tfd`-object

- [`tf_spread()`](https://tidyfun.github.io/tidyfun/reference/tf_spread.md)
  :

  Spread a `tf`-column into many columns representing the function
  evaluations.

- [`tf_nest()`](https://tidyfun.github.io/tidyfun/reference/tf_nest.md)
  :

  Turn "long" tables into tidy data frames with `tf`-objects

- [`tf_unnest()`](https://tidyfun.github.io/tidyfun/reference/tf_unnest.md)
  :

  Turn (data frames with) `tf`-objects / list columns into "long"
  tables.

- [`tf_evaluate(`*`<data.frame>`*`)`](https://tidyfun.github.io/tidyfun/reference/tf_evaluate.data.frame.md)
  :

  Evaluate `tf`s inside a `data.frame`

## tidyfun graphics and display

Plotting helpers and display methods provided by tidyfun

- [`tf_ggplot()`](https://tidyfun.github.io/tidyfun/reference/tf_ggplot.md)
  : Create a tf-aware ggplot

- [`is_tf_ggplot()`](https://tidyfun.github.io/tidyfun/reference/is_tf_ggplot.md)
  : Check if object is a tf_ggplot

- [`parse_tf_aesthetics()`](https://tidyfun.github.io/tidyfun/reference/parse_tf_aesthetics.md)
  : Parse aesthetic mappings to separate tf and regular aesthetics

- [`` `+`( ``*`<tf_ggplot>`*`)`](https://tidyfun.github.io/tidyfun/reference/plus-.tf_ggplot.md)
  : Add layers to tf_ggplot objects

- [`print(`*`<tf_ggplot>`*`)`](https://tidyfun.github.io/tidyfun/reference/print.tf_ggplot.md)
  : Print method for tf_ggplot

- [`ggplot_build(`*`<tf_ggplot>`*`)`](https://tidyfun.github.io/tidyfun/reference/ggplot_build.tf_ggplot.md)
  : ggplot_build method for tf_ggplot

- [`stat_tf()`](https://tidyfun.github.io/tidyfun/reference/ggspaghetti.md)
  [`geom_spaghetti()`](https://tidyfun.github.io/tidyfun/reference/ggspaghetti.md)
  [`geom_meatballs()`](https://tidyfun.github.io/tidyfun/reference/ggspaghetti.md)
  :

  Spaghetti plots for `tf` objects

- [`gglasagna()`](https://tidyfun.github.io/tidyfun/reference/gglasagna.md)
  :

  Lasagna plots for `tf`s using `ggplot2`

- [`stat_errorband()`](https://tidyfun.github.io/tidyfun/reference/ggerrorband.md)
  [`geom_errorband()`](https://tidyfun.github.io/tidyfun/reference/ggerrorband.md)
  :

  Error bands using `tf` objects as bounds

- [`stat_capellini()`](https://tidyfun.github.io/tidyfun/reference/ggcapellini.md)
  [`geom_capellini()`](https://tidyfun.github.io/tidyfun/reference/ggcapellini.md)
  :

  Glyph plots for `tf` objects

- [`stat_fboxplot()`](https://tidyfun.github.io/tidyfun/reference/ggfboxplot.md)
  [`geom_fboxplot()`](https://tidyfun.github.io/tidyfun/reference/ggfboxplot.md)
  :

  Functional boxplots for `tf` objects

- [`type_sum(`*`<tf>`*`)`](https://tidyfun.github.io/tidyfun/reference/tftibble.md)
  [`obj_sum(`*`<tf>`*`)`](https://tidyfun.github.io/tidyfun/reference/tftibble.md)
  [`pillar_shaft(`*`<tf>`*`)`](https://tidyfun.github.io/tidyfun/reference/tftibble.md)
  : Format tidy functional data for tibbles

## Related tf classes and conversion

Core functional-data classes and constructors documented for convenience

- [`tfd()`](https://tidyfun.github.io/tf/reference/tfd.html)
  [`as.tfd()`](https://tidyfun.github.io/tf/reference/tfd.html)
  [`as.tfd_irreg()`](https://tidyfun.github.io/tf/reference/tfd.html) :
  Constructors for vectors of "raw" functional data (from tf)

- [`tfd()`](https://tidyfun.github.io/tf/reference/tfd.html)
  [`as.tfd()`](https://tidyfun.github.io/tf/reference/tfd.html)
  [`as.tfd_irreg()`](https://tidyfun.github.io/tf/reference/tfd.html) :
  Constructors for vectors of "raw" functional data (from tf)

- [`tfb()`](https://tidyfun.github.io/tf/reference/tfb.html)
  [`tfb_wavelet()`](https://tidyfun.github.io/tf/reference/tfb.html)
  [`as.tfb()`](https://tidyfun.github.io/tf/reference/tfb.html) :
  Constructors for functional data in basis representation (from tf)

- [`tfb()`](https://tidyfun.github.io/tf/reference/tfb.html)
  [`tfb_wavelet()`](https://tidyfun.github.io/tf/reference/tfb.html)
  [`as.tfb()`](https://tidyfun.github.io/tf/reference/tfb.html) :
  Constructors for functional data in basis representation (from tf)

- [`as.data.frame(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/converters.html)
  [`as.matrix(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/converters.html)
  [`as.function(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/converters.html)
  : Convert functional data back to tabular data formats (from tf)

- [`tfb_spline()`](https://tidyfun.github.io/tf/reference/tfb_spline.html)
  : Spline-based representation of functional data (from tf)

- [`tfb_fpc()`](https://tidyfun.github.io/tf/reference/tfb_fpc.html) :
  Functional data in FPC-basis representation (from tf)

- [`fpc_wsvd()`](https://tidyfun.github.io/tf/reference/fpc_wsvd.html) :
  Eigenfunctions via weighted, regularized SVD (from tf)

- [`tf_rebase()`](https://tidyfun.github.io/tf/reference/tf_rebase.html)
  :

  Change (basis) representation of a `tf`-object (from tf)

## Related tf evaluation and computation

Indexing, summary, and calculus helpers documented for convenience

- [`` `[`( ``*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfbrackets.html)
  [`` `[<-`( ``*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfbrackets.html)
  :

  Accessing, evaluating, subsetting and subassigning `tf` vectors (from
  tf)

- [`tf_approx_linear()`](https://tidyfun.github.io/tf/reference/tf_approx.html)
  [`tf_approx_spline()`](https://tidyfun.github.io/tf/reference/tf_approx.html)
  [`tf_approx_none()`](https://tidyfun.github.io/tf/reference/tf_approx.html)
  [`tf_approx_fill_extend()`](https://tidyfun.github.io/tf/reference/tf_approx.html)
  [`tf_approx_locf()`](https://tidyfun.github.io/tf/reference/tf_approx.html)
  [`tf_approx_nocb()`](https://tidyfun.github.io/tf/reference/tf_approx.html)
  :

  Inter- and extrapolation functions for `tfd`-objects (from tf)

- [`tf_evaluate()`](https://tidyfun.github.io/tf/reference/tf_evaluate.html)
  :

  Evaluate `tf`-vectors for given argument values (from tf)

- [`tf_interpolate()`](https://tidyfun.github.io/tf/reference/tf_interpolate.html)
  :

  Re-evaluate `tf`-objects on a new grid of argument values. (from tf)

- [`tf_split()`](https://tidyfun.github.io/tf/reference/tf_splitcombine.html)
  [`tf_combine()`](https://tidyfun.github.io/tf/reference/tf_splitcombine.html)
  : Split / Combine functional fragments (from tf)

- [`` `==`( ``*`<tfd>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.html)
  [`` `!=`( ``*`<tfd>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.html)
  [`` `==`( ``*`<tfb>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.html)
  [`` `!=`( ``*`<tfb>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.html)
  [`vec_arith(`*`<tfd>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.html)
  [`vec_arith(`*`<tfb>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.html)
  [`Math(`*`<tfd>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.html)
  [`Math(`*`<tfb>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.html)
  [`Summary(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.html)
  [`cummax(`*`<tfd>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.html)
  [`cummin(`*`<tfd>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.html)
  [`cumsum(`*`<tfd>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.html)
  [`cumprod(`*`<tfd>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.html)
  [`cummax(`*`<tfb>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.html)
  [`cummin(`*`<tfb>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.html)
  [`cumsum(`*`<tfb>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.html)
  [`cumprod(`*`<tfb>`*`)`](https://tidyfun.github.io/tf/reference/tfgroupgenerics.html)
  :

  Math, Summary and Ops Methods for `tf` (from tf)

- [`mean(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfsummaries.html)
  [`median(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfsummaries.html)
  [`sd()`](https://tidyfun.github.io/tf/reference/tfsummaries.html)
  [`var()`](https://tidyfun.github.io/tf/reference/tfsummaries.html)
  [`summary(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfsummaries.html)
  :

  Functions that summarize `tf` objects across argument values (from tf)

- [`tf_depth()`](https://tidyfun.github.io/tf/reference/tf_depth.html) :
  Functional Data Depth (from tf)

- [`tf_fwise()`](https://tidyfun.github.io/tf/reference/functionwise.html)
  [`tf_fmax()`](https://tidyfun.github.io/tf/reference/functionwise.html)
  [`tf_fmin()`](https://tidyfun.github.io/tf/reference/functionwise.html)
  [`tf_fmedian()`](https://tidyfun.github.io/tf/reference/functionwise.html)
  [`tf_frange()`](https://tidyfun.github.io/tf/reference/functionwise.html)
  [`tf_fmean()`](https://tidyfun.github.io/tf/reference/functionwise.html)
  [`tf_fvar()`](https://tidyfun.github.io/tf/reference/functionwise.html)
  [`tf_fsd()`](https://tidyfun.github.io/tf/reference/functionwise.html)
  [`tf_crosscov()`](https://tidyfun.github.io/tf/reference/functionwise.html)
  [`tf_crosscor()`](https://tidyfun.github.io/tf/reference/functionwise.html)
  :

  Summarize each `tf` in a vector (function-wise) (from tf)

- [`tf_derive()`](https://tidyfun.github.io/tf/reference/tf_derive.html)
  : Differentiating functional data: approximating derivative functions
  (from tf)

- [`tf_integrate()`](https://tidyfun.github.io/tf/reference/tf_integrate.html)
  : Integrals and anti-derivatives of functional data (from tf)

- [`tf_smooth()`](https://tidyfun.github.io/tf/reference/tf_smooth.html)
  :

  Simple smoothing of `tf` objects (from tf)

## Related tf registration and utilities

Registration, search, random generation, and helper functions documented
for convenience

- [`tf_register()`](https://tidyfun.github.io/tf/reference/tf_register.html)
  :

  Register / align a `tf` vector against a template function (from tf)

- [`tf_landmarks_extrema()`](https://tidyfun.github.io/tf/reference/landmarks.html)
  [`detect_landmarks()`](https://tidyfun.github.io/tf/reference/landmarks.html)
  [`cluster_landmarks()`](https://tidyfun.github.io/tf/reference/landmarks.html)
  [`build_landmark_matrix()`](https://tidyfun.github.io/tf/reference/landmarks.html)
  : Find Extrema Locations in Functional Data (from tf)

- [`tf_warp()`](https://tidyfun.github.io/tf/reference/tf_warp.html) :

  Elastic Deformation: warp and align `tf` vectors (from tf)

- [`tf_invert()`](https://tidyfun.github.io/tf/reference/tf_invert.html)
  :

  Invert a `tf` vector (from tf)

- [`tf_align()`](https://tidyfun.github.io/tf/reference/tf_align.html) :
  Apply warping functions to align functional data (from tf)

- [`tf_estimate_warps()`](https://tidyfun.github.io/tf/reference/tf_estimate_warps.html)
  : Estimate warping functions for registration (from tf)

- [`tf_where()`](https://tidyfun.github.io/tf/reference/tf_where.html)
  [`tf_anywhere()`](https://tidyfun.github.io/tf/reference/tf_where.html)
  : Find out where functional data fulfills certain conditions. (from
  tf)

- [`tf_zoom()`](https://tidyfun.github.io/tf/reference/tf_zoom.html) :
  Functions to zoom in/out on functions (from tf)

- [`tf_arg()`](https://tidyfun.github.io/tf/reference/tfmethods.html)
  [`tf_evaluations()`](https://tidyfun.github.io/tf/reference/tfmethods.html)
  [`tf_count()`](https://tidyfun.github.io/tf/reference/tfmethods.html)
  [`tf_domain()`](https://tidyfun.github.io/tf/reference/tfmethods.html)
  [`` `tf_domain<-`() ``](https://tidyfun.github.io/tf/reference/tfmethods.html)
  [`tf_evaluator()`](https://tidyfun.github.io/tf/reference/tfmethods.html)
  [`` `tf_evaluator<-`() ``](https://tidyfun.github.io/tf/reference/tfmethods.html)
  [`tf_basis()`](https://tidyfun.github.io/tf/reference/tfmethods.html)
  [`` `tf_arg<-`() ``](https://tidyfun.github.io/tf/reference/tfmethods.html)
  [`coef(`*`<tfb>`*`)`](https://tidyfun.github.io/tf/reference/tfmethods.html)
  [`rev(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfmethods.html)
  [`is.na(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfmethods.html)
  [`is.na(`*`<tfd_irreg>`*`)`](https://tidyfun.github.io/tf/reference/tfmethods.html)
  [`is_tf()`](https://tidyfun.github.io/tf/reference/tfmethods.html)
  [`is_tfd()`](https://tidyfun.github.io/tf/reference/tfmethods.html)
  [`is_reg()`](https://tidyfun.github.io/tf/reference/tfmethods.html)
  [`is_tfd_reg()`](https://tidyfun.github.io/tf/reference/tfmethods.html)
  [`is_irreg()`](https://tidyfun.github.io/tf/reference/tfmethods.html)
  [`is_tfd_irreg()`](https://tidyfun.github.io/tf/reference/tfmethods.html)
  [`is_tfb()`](https://tidyfun.github.io/tf/reference/tfmethods.html)
  [`is_tfb_spline()`](https://tidyfun.github.io/tf/reference/tfmethods.html)
  [`is_tfb_fpc()`](https://tidyfun.github.io/tf/reference/tfmethods.html)
  :

  Utility functions for `tf`-objects (from tf)

- [`tf_rgp()`](https://tidyfun.github.io/tf/reference/tf_rgp.html) :
  Gaussian Process random generator (from tf)

- [`tf_jiggle()`](https://tidyfun.github.io/tf/reference/tf_jiggle.html)
  [`tf_sparsify()`](https://tidyfun.github.io/tf/reference/tf_jiggle.html)
  :

  Make a `tf` (more) irregular (from tf)

- [`in_range()`](https://tidyfun.github.io/tf/reference/in_range.html)
  [`` `%inr%` ``](https://tidyfun.github.io/tf/reference/in_range.html)
  : Find out if values are inside given bounds (from tf)

- [`ensure_list()`](https://tidyfun.github.io/tf/reference/ensure_list.html)
  : Turns any object into a list (from tf)

- [`unique_id()`](https://tidyfun.github.io/tf/reference/unique_id.html)
  : Make syntactically valid unique names (from tf)

- [`vec_cast(`*`<tfd_reg.tfd_reg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_cast(`*`<tfd_reg.tfd_irreg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_cast(`*`<tfd_reg.tfb_spline>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_cast(`*`<tfd_reg.tfb_fpc>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_cast(`*`<tfd_irreg.tfd_reg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_cast(`*`<tfd_irreg.tfd_irreg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_cast(`*`<tfd_irreg.tfb_spline>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_cast(`*`<tfd_irreg.tfb_fpc>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_cast(`*`<tfb_spline.tfb_spline>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_cast(`*`<tfb_spline.tfb_fpc>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_cast(`*`<tfb_fpc.tfb_spline>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_cast(`*`<tfb_fpc.tfb_fpc>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_cast(`*`<tfb_spline.tfd_reg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_cast(`*`<tfb_spline.tfd_irreg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_cast(`*`<tfb_fpc.tfd_reg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_cast(`*`<tfb_fpc.tfd_irreg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_ptype2(`*`<tfd_reg.tfd_reg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_ptype2(`*`<tfd_reg.tfd_irreg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_ptype2(`*`<tfd_reg.tfb_spline>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_ptype2(`*`<tfd_reg.tfb_fpc>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_ptype2(`*`<tfd_irreg.tfd_reg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_ptype2(`*`<tfd_irreg.tfd_irreg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_ptype2(`*`<tfd_irreg.tfb_spline>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_ptype2(`*`<tfd_irreg.tfb_fpc>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_ptype2(`*`<tfb_spline.tfb_spline>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_ptype2(`*`<tfb_spline.tfb_fpc>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_ptype2(`*`<tfb_spline.tfd_reg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_ptype2(`*`<tfb_spline.tfd_irreg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_ptype2(`*`<tfb_fpc.tfb_spline>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_ptype2(`*`<tfb_fpc.tfb_fpc>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_ptype2(`*`<tfb_fpc.tfd_reg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  [`vec_ptype2(`*`<tfb_fpc.tfd_irreg>`*`)`](https://tidyfun.github.io/tf/reference/vctrs.html)
  :

  `vctrs` methods for `tf` objects (from tf)

- : tf: S3 Classes and Methods for Tidy Functional Data (from tf)

## Related tf display methods

Core tf display helpers documented for convenience

- [`autoplot(`*`<tf>`*`)`](https://tidyfun.github.io/tidyfun/reference/autoplot.tf.md)
  [`autolayer(`*`<tf>`*`)`](https://tidyfun.github.io/tidyfun/reference/autoplot.tf.md)
  :

  Autoplot and autolayer methods for `tf` objects

- [`plot(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfviz.html)
  [`lines(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfviz.html)
  [`points(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfviz.html)
  :

  `base` plots for `tf`s (from tf)

- [`print(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfdisplay.html)
  [`print(`*`<tfd_reg>`*`)`](https://tidyfun.github.io/tf/reference/tfdisplay.html)
  [`print(`*`<tfd_irreg>`*`)`](https://tidyfun.github.io/tf/reference/tfdisplay.html)
  [`print(`*`<tfb>`*`)`](https://tidyfun.github.io/tf/reference/tfdisplay.html)
  [`format(`*`<tf>`*`)`](https://tidyfun.github.io/tf/reference/tfdisplay.html)
  : Pretty printing and formatting for functional data (from tf)

- [`prep_plotting_arg()`](https://tidyfun.github.io/tf/reference/prep_plotting_arg.html)
  : Preprocess evaluation grid for plotting (from tf)

## Related tf datasets

Functional data sets documented for convenience

- [`gait`](https://tidyfun.github.io/tf/reference/gait.html) : Hip and
  knee angle while walking data (from tf)
- [`growth`](https://tidyfun.github.io/tf/reference/growth.html) :
  Berkeley growth study data (from tf)
- [`pinch`](https://tidyfun.github.io/tf/reference/pinch.html) : Pinch
  force data (from tf)

## tidyfun datasets

Functional data sets shipped with tidyfun

- [`chf_df`](https://tidyfun.github.io/tidyfun/reference/chf_df.md) :
  Congestive heart failure accelerometry data
- [`dti_df`](https://tidyfun.github.io/tidyfun/reference/dti_df.md) :
  Diffusion tensor imaging data
