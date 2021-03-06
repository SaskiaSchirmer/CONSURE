% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_f_sub.R
\name{f_f_sub}
\alias{f_f_sub}
\title{subdensity function for recovered individuals}
\usage{
f_f_sub(w, t, b, markRecaptureObject)
}
\arguments{
\item{w}{decimal number (1D) or vector of decimal numbers (2D): spatial point
of recovery}

\item{t}{integer: temporal component of recovery}

\item{b}{specifies breeding area for which the plot is drawn. Can be either
a breedingAreaName, the corresponding number of the breeding area or "all"
for all breeding areas at once.}

\item{markRecaptureObject}{object of class markRecaptureObject
(see markRecaptureObject())}
}
\value{
subdensity of recovered individuals for the specified parameters
}
\description{
This function sets up the subdensity function for every spatio-temporal point
for recovered individuals by means of given survival, migratory connectivity
and recovery probability.
}
\examples{
ffs <- f_f_sub(1, 1, 1, mro1D)
}
