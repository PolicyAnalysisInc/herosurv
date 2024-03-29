% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surv_lifetable.r
\name{define_surv_lifetable}
\alias{define_surv_lifetable}
\title{Define Life-Table Distribution}
\usage{
define_surv_lifetable(
  x,
  start_age,
  percent_male,
  output_unit = "years",
  age_col = "age",
  male_col = "male",
  female_col = "female",
  dpy = get_dpy(),
  percent_female = NULL
)
}
\arguments{
\item{x}{a data frame with columns for the starting age in each age band, the
conditional probability of death at each age for men, and the conditional probability
of death at each for women. Default column names are "age", "male", and "female", but can
be set via optional arguments.}

\item{start_age}{starting age of the population.}

\item{percent_male}{percent of population that is male. Calculated as 1 - percent_female if not provided.}

\item{output_unit}{optional arguemnt for time unit resulting survival distribution will be defined
in terms of. Valid options are \code{c("days", "weeks", "months", "years")}. Defaults to \code{"years"}.}

\item{age_col}{optional argument to change name of the \code{age} column accepted by the
first argument.}

\item{male_col}{optional argument to change name of the \code{male} column accepted by the
first argument.}

\item{female_col}{optional argument to change name of the \code{female} column accepted by the
first argument.}

\item{dpy}{optional argument specifying the number of days per year used in time unit
conversion. This argument will be populated automatically in a heromod model.}

\item{percent_female}{percent of population that is female. Calculated as 1 - percent_male if not provided.}
}
\value{
a \code{surv_lifetable} object.
}
\description{
Define a survival distribution based on a life-table containing mortality rates by age
and gender.
}
\examples{
 x <- data.frame(
     age = c(0, 1, 2, 3),
     male = c(0.011, 0.005, 0.003, 0.002),
     female = c(0.010, 0.005, 0.004, 0.002)
 )
 define_surv_lifetable(x, 1, 0.45)
 
}
