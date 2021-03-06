% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitBaselineRegularization.R
\name{fitBaselineRegularization}
\alias{fitBaselineRegularization}
\title{Fit baseline regularization}
\usage{
fitBaselineRegularization(br_data, parameters = defineBRParameters())
}
\arguments{
\item{br_data}{data object produced by [prepareBRData]}

\item{parameters}{parameters object produced by [defineBRParameters]}
}
\value{
An object representing the fit model, with elements:
* beta, the model coefficients
* t
* res
* parameters, (the `parameters` passed to this function)
* drug_concept_id, vector of drug concept IDs, for matching back with beta coefficients
}
\description{
Fit Baseline Regularization with the given parameters
}
\author{
Zhaobin Kuang
}
