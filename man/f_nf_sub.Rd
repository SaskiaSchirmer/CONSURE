% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_nf_sub.R
\name{f_nf_sub}
\alias{f_nf_sub}
\title{subdensity function for not seen individuals}
\usage{
f_nf_sub(w, b, markRecaptureObject)
}
\arguments{
\item{w}{decimal number (1D) or vector of decimal numbers (2D): spatial point
of recovery}

\item{b}{specifies breeding area for which the plot is drawn. Can be either
a breedingAreaName, the corresponding number of the breeding area or "all"
for all breeding areas at once.}

\item{markRecaptureObject}{object of class markRecaptureObject
(see markRecaptureObject())}
}
\value{
subdensity of not seen individuals for the specified parameters
}
\description{
This function sets up the subdensity function for every spatio-temporal point
for not seen individuals by means of given survival, migratory connectivity
and recovery probability. Not seen individuals died in observation time but
were not found or survived observation time.
}
\examples{
fnfs <- f_nf_sub(1, 1, mro1D)
}
