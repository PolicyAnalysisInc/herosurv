% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surv_cure.r
\name{define_surv_cure}
\alias{define_surv_cure}
\alias{define_survival_cure}
\title{Define Parametric Mixture or Non-Mixture Cure Distribution}
\usage{
define_surv_cure(distribution, theta, ..., mixture = TRUE)

define_survival_cure(distribution, theta, ..., mixture = TRUE)
}
\arguments{
\item{distribution}{A parametric survival distribution. See
details for a listing of valid distributions.}

\item{theta}{A numeric value representing cure fraction}

\item{...}{Additional distribution parameters
(see details section below)}

\item{mixture}{a logical determining whether a mixture
or non-mixture model is being defined.}
}
\value{
a \code{surv_dist_cure} object.
}
\description{
Define parametric mixture or not mixture cure distribution
with given parameter values.
}
\details{
Supported distributions are listed in the table below.\tabular{llll}{
   \strong{Distribution} \tab \strong{Description} \tab \strong{Parameters} \tab \strong{Notes} \cr
   "exp" \tab Exponential \tab rate \tab  \cr
   "lnorm" \tab Lognormal \tab meanlog, sdlog \tab  \cr
   "llogis" \tab Log-Logistic \tab shape, scale \tab  \cr
   "weibull" \tab Weibull (AFT) \tab shape, scale \tab  \cr
   "weibullPH" \tab Weibull (PH) \tab shape, scale \tab  \cr
   "gompertz" \tab Gompertz \tab shape, rate \tab  \cr
   "gamma" \tab Gamma \tab shape, scale \tab  \cr
   "gengamma" \tab Generalized Gamma (stable) \tab mu, sigma, Q \tab Parameterization from Prentice (1974) \cr
   "gengamma.orig" \tab Generalized Gamma (original) \tab shape, scale, k \tab Original parameterization from Stacy (1962) \cr
   "genf" \tab Generalized F (stable) \tab mu, sigma, Q, P \tab Stable reparameterization from Prentice (1975) \cr
   "genf.org" \tab Generalized F (original) \tab mu, sigma, s1, s2 \tab Origninal parameterization described by Prentice (1975) \cr
}
}
\examples{

define_surv_cure(distribution = "exp", theta = 0.34, rate = .5)
define_surv_cure(
 distribution = "weibull",
 theta = 0.5, shape = 1.5,
 scale = 34.43,
 mixture = TRUE
)
# Deprecated alias included for backwards compatability with heRomod
define_survival_cure(distribution = "exp", theta = 0.24, rate = 0.023)

}
\references{
Stacy, E. W. (1962). A generalization of the gamma
distribution.  Annals of Mathematical Statistics 33:1187-92.

Prentice, R. L. (1974). A log gamma model and its maximum likelihood
estimation. Biometrika 61(3):539-544.

R. L. Prentice (1975). Discrimination among some parametric
models. Biometrika 62(3):607-614.
}
