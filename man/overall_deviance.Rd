% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{overall_deviance}
\alias{overall_deviance}
\title{Calculate the overall deviance}
\usage{
overall_deviance(params, split_data, ..., priors = list())
}
\arguments{
\item{params}{A vector of parameters.}

\item{split_data}{a data.frame split by \code{split()}}

\item{...}{additional arguments to pass to calcdev()}

\item{priors}{a named list}
}
\value{
The overall deviance.
}
\description{
This function calculates the overall deviance for a given set of parameters and data.
It calculates the deviance for each list entry of split_data and then sums them
}
