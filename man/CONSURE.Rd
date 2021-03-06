% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CONSURE.R
\docType{package}
\name{CONSURE}
\alias{CONSURE}
\title{CONSURE: Spatially continuous
survival, use of space and recovery probability estimates}
\description{
The CONSURE package provides functions to perform the continuous and the
combined approach from the dissertation of Saskia Schirmer. It can work with
both, simulated and real-world data.
}
\details{
Nearly all functions are based on a \code{\link{markRecaptureObject}}.

A spatial point pattern can be simulated by given survival, migratory
connectivity and recovery probability using \code{\link{simContin}}.
}
\section{Visualizing raw data}{


Spatial point patterns of raw recoveries can be visualized by
\code{\link{plotRawRecoveries}}. The age distribution can be visualized by
\code{\link{plotAgeDistribution}}.
}

\section{Continuous functions}{


The continuous estimation approach first estimates the density of the point
pattern by \code{\link{estKDE}}. Then, survival can be estimated by
\code{\link{estS}} and finally migratory connectivity by \code{\link{estM}}
and a constant recovery probability by \code{\link{estR}}.

All estimates can be plotted by the appropriate function:
\code{\link{plotKDE}}, \code{\link{plotS}}, \code{\link{plotM}}. The R^2
values of the linear model used to fit survival, migratory connectivity and
recovery probability can be plotted by \code{\link{plotGOFofLM}}.

The number of recovered individuals per breeding area can be summarized by
\code{\link{recIndsFunc}}.

\code{\link{par_grid}} creates a grid containing the values of a specific
function on the grid.
}

