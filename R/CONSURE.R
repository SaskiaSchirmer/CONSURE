#' CONSURE: Spatially continuous
#' survival, use of space and recovery probability estimates
#'
#' The CONSURE package provides functions to perform the
#' continuous and the combined approach from the dissertation of Saskia Schirmer.
#' It can work with both, simulated and real-world data.
#'
#' Nearly all functions are based on a \code{\link{markRecaptureObject}}.
#'
#' A spatial point pattern can be simulated by given survival, migratory connectivity
#' and recovery probability using \code{\link{simContin}}.
#'
#' @section Visualizing raw data:
#'
#' Spatial point patterns of raw recoveries can be visualized by \code{\link{plotRawRecoveries}}.
#' The age distribution can be visualized by \code{\link{plotAgeDistribution}}.
#'
#' @section Continuous functions:
#'
#' The continuous estimation approach first estimates the density of the point pattern
#' by \code{\link{estKDE}}. Then, survival can be estimated by \code{\link{estS}} and finally
#' migratory connectivity by \code{\link{estM}} and a constant recovery probability by
#'  \code{\link{estR}}.
#'
#'  All estimates can be plotted by the appropriate function:
#'  \code{\link{plotKDE}}, \code{\link{plotS}}, \code{\link{plotM}}.
#'  The R^2 values of the linear model used to fit survival, migratory connectivity and
#'  recovery probability can be plotted by \code{\link{plotGOFofLM}}.
#'
#
#'
#'
#'
#'
#' @section Combined functions:
#'
#' If recovery probability is not constant over space, an extra
#' \code{\link{optimizationObject}} is needed. The prop argument
#' must be either specified, e.g., by a discrete estimate of
#' migratory connectivity for real-world data or simulated data (not included in this package at the moment),
#' or the true discrete migratory connectivity values can be calculated
#' by \code{\link{calcDiscreteM}} and forwarded by the markRecaptureObject.
#'
#' Migratory connectivity and recovery probability can then be estimated
#' spatially continuously by \code{\link{combEstimate}}.
#'
#' The combined estimates can be plotted by \code{\link{plotCombM}} and \code{\link{plotCombR}}.
#'
#' @section Additional functions:
#'
#' All other functions should not be relevant to the user. For a short overview:
#'
#' A markRecaptureObject consists of a breedingAreaObject and a winteringAreaObject.
#' The constructors \code{\link{new_markRecaptureObject}},
#' \code{\link{new_breedingArea}}, \code{\link{new_winteringArea}} are accessed via their
#'  helper functions \code{\link{markRecaptureObject}}, \code{\link{breedingArea}}, \code{\link{winteringArea}}.
#'  Also the constructor of the optimizationObject \code{\link{new_optimizationObject}} is accessed via the
#'  helper function \code{\link{optimizationObject}}.
#'
#'  To define the density \code{\link{f_f}} of the point pattern correctly, the subdensity
#'  of found \code{\link{f_f_sub}} individuals is needed.
#'  The probability to be not seen \code{\link{p_nf}} is integrated over the subdensity
#'   of not found \code{\link{f_nf_sub}} individuals.
#'
#'  Estimating survival and migratory connectivity in the continuous approach is based on a
#'  linear model which is estimated by \code{\link{estLM}}.
#'
#'  The optimization procedure initializes B-splines via \code{\link{initSpline}} and
#'  defines B-splines by \code{\link{defineBspline}}. The penalizing function
#'  \code{\link{pen}} integrates the distance to the continuous estimate via
#'  \code{\link{integrateDist2Continuous}}
#'   and the distance to the discrete estimate via \code{\link{integrateDist2Discrete}}.
#'   The smoothness penalty is implemented in \code{\link{Lh}}.
#'
#'   The number of recovered individuals per breeding area can be summarized by \code{\link{recIndsFunc}}.
#'
#'   \code{\link{par_grid}} creates a grid containing the values of a specific function on the grid.
#'
#'
#'
#' @docType package
#' @name CONSURE
NULL
#> NULL
