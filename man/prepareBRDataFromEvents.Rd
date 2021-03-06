% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepareBRDataFromEvents.R
\name{prepareBRDataFromEvents}
\alias{prepareBRDataFromEvents}
\title{Generate BR data from event table ( internal function )}
\usage{
prepareBRDataFromEvents(all_events, event, tying)
}
\arguments{
\item{all_events}{Table of events with columns:
`obs_period_id`,
`event_day`,
`concept_id`,
`event_flag`,
`observation_period_length`}

\item{event}{The event of interest the risk of which to estimate.}

\item{tying}{The type of tying to use (`interval` or `occurrence`).}
}
\value{
Data for [fitBaselineRegularization].
}
\description{
Internal function that prepares date for [fitBaselineRegularization] from the output of a `getEventsFrom*` function.
This version uses functions not supported by SQLite.
}
\author{
Yuriy Sverchkov
}
