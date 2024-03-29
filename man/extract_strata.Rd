% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surv_survfit.r
\name{extract_strata}
\alias{extract_strata}
\title{Extract Product-Limit Tables}
\usage{
extract_strata(sf)
}
\arguments{
\item{sf}{A survit object.}
}
\value{
A tidy data.frame of the product-limit tables for
all strata.
}
\description{
Extracts the product-limit table from a survfit object
for all strata. Only \code{survfit} and unstratified
\code{survfit.coxph} objects are supported.
}
\keyword{internal}
