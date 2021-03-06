% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estKDE.R
\name{estKDE}
\alias{estKDE}
\title{estimate raw density of recovered individuals}
\usage{
estKDE(
  markRecaptureObject,
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
\item{markRecaptureObject}{object of class markRecaptureObject
(see markRecaptureObject())}

\item{res}{resolution in space}

\item{all}{boolean: if TRUE only one kernel density estimate will be
calculated summarizing all breeding areas. Defaults to FALSE.}

\item{xname}{name of x variable, e.g. longitude}

\item{yname}{name of y variable, e.g. latitude}

\item{timename}{name of time variable, e.g. age}

\item{bw}{numeric. Spatial bandwidth parameter. Defaults to NULL.}

\item{lam}{numeric. temporal bandwidth parameter. Defaults to 1.1.}
}
\value{
markRecaptureObject with list of values created by
sparr::spattemp.density (see ?sparr::spattemp.density for details) and
spatial resolution.
}
\description{
This function estimates the raw density of recovered individuals in space
and time from given data using a kernel density estimate.
}
\examples{
mro <- estKDE(mro1D)
}
