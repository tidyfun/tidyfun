% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidyfun-package.R
\docType{package}
\name{tidyfun-package}
\alias{tidyfun}
\alias{tidyfun-package}
\title{tidyfun: Tools for Tidy Functional Data}
\description{
\code{tidyfun} makes data wrangling and exploratory analysis for functional data
in \code{R} easier.\cr\cr
\code{tidyfun} is based on the classes and methods defined in package \code{tf} and provides:
\itemize{
\item new data types for representing functional data: \code{\link[tf:tfd]{tf::tfd()}} & \code{\link[tf:tfb]{tf::tfb()}}
\item arithmetic operators (\code{\link[tf:tfgroupgenerics]{tf::Ops.tf()}}),
\item descriptive statistics: e.g. \code{\link[tf:tfsummaries]{tf::mean.tf()}}, \code{\link[tf:tfsummaries]{tf::median.tf()}}
\item and graphics functions for such data: \code{\link[tf:tfviz]{tf::plot.tf()}}, \code{\link[=geom_spaghetti]{geom_spaghetti()}}, \code{\link[=gglasagna]{gglasagna()}}
\item functions to do smoothing (\code{\link[tf:tf_smooth]{tf::tf_smooth.tfd()}}), differentiation and integration (\code{\link[tf:tf_derive]{tf::tf_derive.tfd()}})
\item \code{tidyverse}-verbs for handling functional data inside data frames: \code{\link[=tf_gather]{tf_gather()}} etc.
\cr\cr
\cr
Also see \code{vignette("Introducing tidyfun", "tidyfun")} for a brief introduction.
}
}
\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/tidyfun/tidyfun}
  \item \url{https://tidyfun.github.io/tidyfun/}
  \item Report bugs at \url{https://github.com/tidyfun/tidyfun/issues}
}

}
\author{
\strong{Maintainer}: Fabian Scheipl \email{fabian.scheipl@googlemail.com} (\href{https://orcid.org/0000-0001-8172-3603}{ORCID})

Authors:
\itemize{
  \item Jeff Goldsmith
  \item Julia Wrobel (\href{https://orcid.org/0000-0001-6783-1421}{ORCID})
}

Other contributors:
\itemize{
  \item Maximilian Muecke (\href{https://orcid.org/0009-0000-9432-9795}{ORCID}) [contributor]
}

}
\keyword{internal}
