% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mro2D.R
\docType{data}
\name{mro2D}
\alias{mro2D}
\title{simulated data for the continuous approach (2D)}
\format{
A markRecaptureObject and a numeric:
\describe{
  \item{mro}{An object of the type markRecaptureObject containing the simulated
  data with the true values of the parameteres and the estimated values of the
  continuous approach}
  \item{res_x}{numeric, resolution for the plots}
}
}
\usage{
data(mro2D)
}
\description{
A dataset containing data simulated with constant recovery probability in
two-dimensional space and continuous estimates of all parameters.
}
\examples{

data(mro2D)
str(mro2D)

}
\references{
This data set was artificially created for the CONSURE package.
}
\keyword{datasets}