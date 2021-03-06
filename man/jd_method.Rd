% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jd_method.R
\name{jd_method}
\alias{jd_method}
\title{Joint density estimation methods}
\usage{
jd_method(package = NULL, fit_fun = NULL, eval_fun = NULL, cc = TRUE,
  ...)
}
\arguments{
\item{package}{character string of the package name; so far, only
\code{"cctools"}, \code{"kdevine"}, \code{"np"} are implemented.}

\item{fit_fun}{only used if \code{package = NULL}; a function of type \code{function(x, ...)} that fits a joint density model on a data matrix \code{x}. The \code{...} can
be used for passing additional parameters.}

\item{eval_fun}{only used if \code{package = NULL}; a function of type
\code{function(object, newdata, ...)} that takes an object fitted by \code{fit_fun}
and evaluates the density estimate on \code{newdata}.}

\item{cc}{only used if \code{package = NULL}; if \code{TRUE}, discrete variables (and
the class indicator) are made continuous with \code{\link[cctools:cont_conv]{cctools::cont_conv()}}; only
use \code{FALSE} when your \code{fit_fun} can handle discrete variables.}

\item{...}{additional parameters passed to \code{fit_fun}.}
}
\value{
An object of class \code{"jd_method"}.
}
\description{
Provides pre-implemented APIs to packages for joint density
estimation. Optionally, custom fit and evaluation functions can be provided.
}
\examples{
# pre-implemented APIs
cctools_method <- jd_method("cctools")
kdevine_method <- jd_method("kdevine")
np_method <- jd_method("np")

# custom API (alternative implementation of kdevine method)
require(kdevine)
fit_kdevine <- function(x, ...)
   kdevine::kdevine(x, ...)
eval_kdevine <- function(object, newdata, ...)
   kdevine::dkdevine(newdata, object)
my_method <- jd_method(fit_fun = fit_kdevine, eval_fun = eval_kdevine, cc = FALSE)

}
