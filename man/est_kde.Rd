% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/est_kde.R
\name{est_kde}
\alias{est_kde}
\title{estimate raw density of recovered individuals}
\usage{
est_kde(
  mark_recapture_object,
  res = 100,
  all = FALSE,
  xname = "longitude",
  yname = "latitude",
  timename = "age",
  bw = NULL,
  lam = 1.1
)
}
\arguments{
\item{mark_recapture_object}{object of class mark_recapture_object
(see mark_recapture_object())}

\item{res}{resolution in space}

\item{all}{boolean: if TRUE only one kernel density estimate will be
calculated summarizing all areas of origin. Defaults to FALSE.}

\item{xname}{name of x variable, e.g. longitude}

\item{yname}{name of y variable, e.g. latitude}

\item{timename}{name of time variable, e.g. age}

\item{bw}{numeric. Spatial bandwidth parameter. Defaults to NULL.}

\item{lam}{numeric. temporal bandwidth parameter. Defaults to 1.1.}
}
\value{
mark_recapture_object with list of values created by
sparr::spattemp.density (see ?sparr::spattemp.density for details) and
spatial resolution.
}
\description{
This function estimates the raw density of recovered individuals in space
and time from given data using a kernel density estimate.
}
\examples{
mro <- est_kde(mro1D)
}
