# CONSURE - Continuous Survival, Use of Space and Recovery Probability
# Estimates.
# Copyright (C) 2021  Saskia Schirmer
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' constructor for wintering area
#'
#' This function defines the properties of the wintering area.
#' @param window object of class "owin":observation window in
#'               two-dimensional plane
#' @param survival function: survival function defined over whole
#'                 wintering area, independent of breeding area
#' @param recovery constant function: recovery probability, must be
#'                 constant over whole wintering area
#' @param recoveryData empty space for simulated or real-world recovery data
#' @return object of class "winteringArea": contains list of window, survival
#'         and recovery for the wintering area


new_winteringArea <- function(window = spatstat.geom::owin(),
                              survival,
                              recovery,
                              recoveryData) {
  stopifnot(spatstat.geom::is.owin(window))
  stopifnot(is.null(survival) | is.function(survival))
  stopifnot(is.null(recovery) | is.function(recovery))
  structure(list(
    window = window, survival = survival, recovery = recovery,
    recoveryData = recoveryData
  ), class = "winteringArea")
}

#' helper function for wintering area
#'
#' This function defines the properties of the wintering area using the
#' constructor.
#' @inheritParams new_winteringArea
#' @param xrange vector in the form of c(xmin,xmax). To define line or rectangle
#' xrange and yrange can be used instead of window.
#' @param yrange vector in the form of c(ymin,ymax). To define line or rectangle
#' xrange and yrange can be used instead of window.
#' @return object of class "winteringArea": contains list of window, survival
#' and recovery for the wintering area
#' @export
#' @examples{
#'      wA <- winteringArea(survival = function(w) 0.3,
#'      recovery = function(w) 0.01,
#'      xrange = c(0,1))
#'  }

winteringArea <- function(window = NULL, survival, recovery, xrange = c(0, 0),
                          yrange = c(0, 0),
                          recoveryData = NULL) {
  try(
    if (is.null(window) &
      identical(xrange, c(0, 0)) &
      identical(yrange, c(0, 0))) {
      stop("Please define either window or x- and/or y-range of wintering area")
    } else {
      if (!spatstat.geom::is.owin(window)) {
        window <- spatstat.geom::as.owin(list(xrange = xrange, yrange = yrange))
      }

      return(new_winteringArea(window, survival, recovery, recoveryData))
    }
  )
}

#' constructor for breeding area
#'
#' This function defines the properties of the breeding area.
#' @param markedInds integer: number of marked individuals in this breeding area
#' @param numberOfRecoveries numeric vector. Number of recoveries belonging to
#' each breeding area.
#' @param migratoryConnectivity function: migratory connectivity function
#' conditioned on this breeding area defined over whole wintering area
#' @return object of class "breedingArea": contains list of number of marked
#' individuals and migratory connectivity function


new_breedingArea <- function(markedInds = numeric(),
                             numberOfRecoveries,
                             migratoryConnectivity) {
  stopifnot(is.null(migratoryConnectivity) |
    is.function(migratoryConnectivity))
  structure(list(
    markedInds = markedInds,
    numberOfRecoveries = numberOfRecoveries,
    migratoryConnectivity = migratoryConnectivity
  ),
  class = "breedingArea"
  )
}

#' helper function for breeding area
#'
#' This function defines the properties of the breeding area using the
#' constructor.
#' @inheritParams new_breedingArea
#' @return object of class "breedingArea": contains list of number of marked
#' individuals and migratory connectivity function
#' @export
#' @examples{
#'  migratoryConnectivity = function(b,w,B=B) {
#'    truncnorm::dtruncnorm(w,0,1, mean = seq(0.1,0.9,length.out = 5)[b],
#'    sd = 0.3)
#'  }
#' migCon <- functional::Curry(migratoryConnectivity,b=1,B=5)
#' bA <- breedingArea(markedInds = 10000,
#'     numberOfRecoveries = NULL,
#'     migratoryConnectivity = migCon)
#' }

breedingArea <- function(markedInds, numberOfRecoveries,
                         migratoryConnectivity) {
  new_breedingArea(markedInds, numberOfRecoveries, migratoryConnectivity)
}

#' constructor for mark recapture object
#'
#' This function defines the properties of the mark recapture object.
#' @param winteringArea object of class "winteringArea"
#' @param breedingAreas list of objects of class "breedingAreas"
#' @param observationTime single integer. length of observation window in years
#' @param numberOfBreedingAreas single integer. number of breeding areas.
#' @param spatialDim single integer. spatial dimensions, should only be 1 or 2.
#' @param robust logical if TRUE robust linear model is calculated to estimate
#' survival and migratory connectivity
#' @return object of class "markRecaptureObject": contains list of wintering
#' area, breeding areas, observationTime, number of breeding areas, spatial
#' dimensions, empty slots for the spatial resolution, the kernel density
#' estimate and the estimates, the class of this object is "markRecaptureObject"

new_markRecaptureObject <- function(winteringArea, breedingAreas,
                                    observationTime, numberOfBreedingAreas,
                                    spatialDim, robust) {
  stopifnot(class(winteringArea) == "winteringArea")
  stopifnot(is.list(breedingAreas))
  stopifnot(observationTime %% 1 == 0)
  stopifnot(length(numberOfBreedingAreas) == 1)
  stopifnot(numberOfBreedingAreas %% 1 == 0)
  stopifnot(is.logical(robust))

  structure(list(
    winteringArea = winteringArea,
    breedingAreas = breedingAreas,
    observationTime = observationTime,
    numberOfBreedingAreas = numberOfBreedingAreas,
    spatialDim = spatialDim,
    spatialResolution = NULL,
    robust = robust,
    kde = list(),
    estimates = list()
  ), class = "markRecaptureObject")
}

#' helper function for mark recapture object
#'
#' This function defines the properties of the mark recapture object using the
#' constructor.
#' @inheritParams winteringArea
#' @param markedInds integer: number of marked individuals in this breeding area
#' @param migratoryConnectivity either a list of functions containing one
#' migratory connectivity function for every breeding area or a function with
#' parameter b, allowing to partialize the function for every breeding area with
#' purrr::partial
#' @param observationTime length of observation window in years
#' @param robust logical if TRUE robust linear model is calculated to estimate
#' survival and migratory connectivity
#' @param realRecoveries real-world recovery data, defaults to NULL
#' @param breedingAreaNames character vector with breeding area names, defaults
#' to NULL
#' @return object of class "markRecaptureObject": contains list of wintering
#' area, breeding areas, observationTime, number of breeding areas and spatial
#' dimension
#' @export
#' @examples{
#' mro <- markRecaptureObject(xrange = c(0,1),
#'     survival = function(w) {0.5*w+.4},
#'     recovery = function(w) {0.01},
#'     markedInds = rep(100000,5) ,
#'     migratoryConnectivity = function(b,w,B=B) {
#'         truncnorm::dtruncnorm(w,0,1, mean = seq(0.1,0.9,length.out = B)[b],
#'         sd = 0.3)
#'         },
#'     observationTime = 10)
#' }

markRecaptureObject <- function(window = NULL,
                                xrange = c(0, 0),
                                yrange = c(0, 0),
                                survival = NULL,
                                recovery = NULL,
                                markedInds,
                                migratoryConnectivity = NULL,
                                observationTime,
                                realRecoveries = NULL,
                                breedingAreaNames = NULL,
                                robust = TRUE) {
  numberOfBreedingAreas <- length(markedInds)

  spatialDim <- 2
  if (is.null(window) & identical(yrange, c(0, 0))) spatialDim <- 1


  if (is.data.frame(realRecoveries)) {
    if (sum(colnames(realRecoveries) %in%
      c("markArea", "longitude", "latitude", "age"))
    != length(colnames(realRecoveries))) {
      message("Your recovery data does not have the default column names.
              You can either use CONSURE::renameData() or you must specify
              the colnames in the functions.")
    }

    tmp <- list()
    for (area in levels(realRecoveries$markArea)) {
      tmp[[area]] <- realRecoveries[realRecoveries$markArea == area, ]
    }
  } else {
    tmp <- realRecoveries
  }


  winteringArea <- winteringArea(window, survival, recovery, xrange, yrange,
    recoveryData = tmp
  )
  breedingAreas <- list()

  if (!is.null(realRecoveries)) {
    numberOfRecoveries <- recIndsFunc(breedingAreaNames, realRecoveries)
  }

  if (is.null(breedingAreaNames)) {
    breedingAreaNames <- paste("b", 1:numberOfBreedingAreas, sep = "")
  }

  if (!is.null(migratoryConnectivity)) {
    if (is.list(migratoryConnectivity)) {
      for (b in 1:numberOfBreedingAreas) {
        breedingAreas[[breedingAreaNames[b]]] <-
          breedingArea(
            markedInds = markedInds[b],
            numberOfRecoveries = NULL,
            migratoryConnectivity = migratoryConnectivity[[b]]
          )
      }
      breedingAreas[["all"]] <-
        breedingArea(
          markedInds = sum(markedInds[b]),
          numberOfRecoveries = NULL,
          migratoryConnectivity = function(w) {
            tmp <- matrix(NA, ncol = length(w), nrow = numberOfBreedingAreas)
            for (b in 1:numberOfBreedingAreas) {
              tmp[b, ] <- markedInds[b] * migratoryConnectivity[[b]](w)
            }
            colSums(tmp) / sum(markedInds)
          }
        )
    } else {
      tmpMig <- list()
      for (b in 1:numberOfBreedingAreas) {
        tmpMig[[b]] <- functional::Curry(migratoryConnectivity,
          b = b,
          B = numberOfBreedingAreas
        )
        breedingAreas[[breedingAreaNames[b]]] <-
          breedingArea(
            markedInds = markedInds[b],
            numberOfRecoveries = NULL,
            migratoryConnectivity = tmpMig[[b]]
          )
      }

      if (spatialDim == 2) {
        breedingAreas[["all"]] <-
          breedingArea(
            markedInds = sum(markedInds),
            numberOfRecoveries = NULL,
            migratoryConnectivity = function(w) {
              tmp <- numeric()
              for (b in 1:numberOfBreedingAreas) {
                tmp[b] <- markedInds[b] * tmpMig[[b]](w)
              }
              sum(tmp) / sum(markedInds)
            }
          )
      } else if (spatialDim == 1) {
        breedingAreas[["all"]] <-
          breedingArea(
            markedInds = sum(markedInds),
            numberOfRecoveries = NULL,
            migratoryConnectivity = Vectorize(
              function(w) {
                tmp <- numeric()
                for (b in 1:numberOfBreedingAreas) {
                  tmp[b] <- markedInds[b] * tmpMig[[b]](w)
                }
                sum(tmp) / sum(markedInds)
              }
            )
          )
      }
    }
  } else {
    for (b in 1:numberOfBreedingAreas) {
      breedingAreas[[breedingAreaNames[b]]] <-
        breedingArea(
          markedInds = markedInds[b],
          numberOfRecoveries = numberOfRecoveries[b],
          migratoryConnectivity = migratoryConnectivity
        )
    }

    breedingAreas[["all"]] <-
      breedingArea(
        markedInds = sum(markedInds),
        numberOfRecoveries = sum(numberOfRecoveries),
        migratoryConnectivity = migratoryConnectivity
      )
  }



  new_markRecaptureObject(
    winteringArea = winteringArea,
    breedingAreas = breedingAreas,
    observationTime = observationTime,
    numberOfBreedingAreas = numberOfBreedingAreas,
    spatialDim = spatialDim,
    robust = robust
  )
}

#' constructor for optimization object
#'
#' This function defines the properties of an optimization object.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param initBeta initial values for the B-spline parameters
#' @param y helper sequence
#' @param knots list of latitude and longitude of knots values for B-spline
#' definition
#' @param b character, name of breeding area to be optimized, for all breeding
#' areas use "all"
#' @param degree degree of B-spline
#' @param lambda numeric vector of 2, weights for the discrete and the
#' smoothness constraint
#' @param split split of the discrete non-breeding areas
#' @param penalize function defining penalization term for optimization
#' @param gradient gradient function for penalty function
#' @param rawSpline spline initialized over helper sequence
#' @param optBeta space for the result of optimization
#' @param values space for the values of the optimization
#' @param inside logical matrix of non-breeding area specifying if a grid cell
#'     is in the non-breeding window or not
#' @return object of class "optimizationObject": contains list of all the
#' parameters

new_optimizationObject <- function(markRecaptureObject,
                                   initBeta,
                                   y,
                                   knots,
                                   b,
                                   degree,
                                   lambda,
                                   split,
                                   penalize,
                                   gradient,
                                   rawSpline,
                                   optBeta = NULL,
                                   values = NULL,
                                   inside) {
  structure(list(
    markRecaptureObject = markRecaptureObject,
    initBeta = initBeta,
    y = y,
    knots = knots,
    b = b,
    degree = degree,
    lambda = lambda,
    split = split,
    penalize = penalize,
    gradient = gradient,
    rawSpline = rawSpline,
    optBeta = optBeta,
    values = values,
    inside = inside
  ), class = "optimizationObject")
}

#' helper function for optimization object
#'
#' This function defines the properties of an optimization object.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param initBeta initial values for the B-spline parameters
#' @param y helper sequence, list of longitude and latitude, latitude defaults
#' to NULL
#' @param knots list of latitude and longitude of knots values for B-spline
#' definition, latitude defaults to NULL
#' @param b character, name of breeding area to be optimized, for all breeding
#' areas use "all"
#' @param degree degree of B-spline
#' @param lambda numeric vector of 2, weights for the discrete and the
#' smoothness constraint
#' @param split split of the discrete non-breeding areas
#' @param useCorrectedM logical, defaults to FALSE. Specifies, if the
#' penalizing function is calculated with the m already corrected for survival
#' and recovery probability. Commonly used for estimating combined migratory
#' connectivity for each breeding area.
#' @param prop defaults to NULL. Proportions for migratory connectivity in
#' discrete space defined by split. Must be specified if they cannot be
#' calculated from the true continuous migratory connectivity function.
#' @return object of class "optimizationObject": contains list of penalization
#' function, rawSpline and optBeta.
#' @examples{
#'  oO <- optimizationObject(markRecaptureObject = mro1DIncreasing$mro,
#'      b = "all",
#'      split = mro1DIncreasing$split,
#'      lambda  = c(.05,300))
#' }
#' @export

optimizationObject <- function(markRecaptureObject, initBeta = NULL,
                               y = list(
                                 longitude =
                                   seq(0, 1,
                                     length.out =
                                       markRecaptureObject$spatialResolution
                                   ),
                                 latitude = NULL
                               ),
                               knots = list(
                                 longitude = seq(0, 1,
                                   length.out = max(
                                     3,
                                     markRecaptureObject$spatialResolution /
                                       10
                                   )
                                 ),
                                 latitude = NULL
                               ),
                               b, degree = 3,
                               lambda = c(0.0001, 10), split,
                               useCorrectedM = FALSE,
                               prop = NULL) {
  innerKnots <- list(
    longitude = knots$longitude[2:(length(knots$longitude) - 1)],
    latitude = knots$latitude[2:(length(knots$latitude) - 1)]
  )

  numberOfInnerKnots <- list(
    longitude = length(innerKnots$longitude),
    latitude = length(innerKnots$latitude)
  )

  print(paste("num", numberOfInnerKnots))

  if (is.null(prop)) {
    if (!is.null(markRecaptureObject$breedingAreas[[b]]$mDiscrete)) {
      prop <- markRecaptureObject$breedingAreas[[b]]$mDiscrete
      prop <- prop / sum(prop)
    } else {
      message("Either define the discrete proportions using prop or calculate
               them from the true continuous distribution using
               calcDiscreteM()")
    }
  } else {
    prop <- prop
  }

  dim <- markRecaptureObject$spatialDim
  res <- markRecaptureObject$spatialResolution

  if (useCorrectedM) {
    m <- markRecaptureObject$estimates[["mCorrected"]][[b]]
  } else {
    m <- markRecaptureObject$estimates[["m"]][[b]]
  }

  inside <- markRecaptureObject$inside

  if (dim == 1) {
    if (is.null(initBeta)) {
      initBeta <- function() {
        stats::rnorm(max(unlist(numberOfInnerKnots)) + degree + 1)
      }
    } else {
      tmp <- initBeta
      initBeta <- function() {
        tmp
      }
    }

    if (sum(sapply(y, is.null)) == 1) {
      y <- unname(unlist(y))
    } else {
      message("not sure how to use 'y'")
    }

    if (sum(sapply(knots, is.null)) == 1) {
      innerKnots <- unname(unlist(innerKnots))
    } else {
      message("not sure how to use 'knots'")
    }

    A <- splines2::dbs(y,
      knots = innerKnots, derivs = 2, degree = degree,
      intercept = TRUE
    )
    A <- A * colSums(inside > 0)
    A_sqrt <- t(A) %*% A
  } else if (dim == 2) {
    if (is.null(initBeta)) {
      initBeta <- function() {
        stats::rnorm((numberOfInnerKnots$longitude + degree + 1) *
          (numberOfInnerKnots$longitude + degree + 1))
      }
    } else {
      tmp <- initBeta
      initBeta <- function() {
        tmp
      }
    }

    A_vv <- splines2::dbs(y$longitude,
      knots = innerKnots$longitude, derivs = 2,
      degree = degree, intercept = TRUE
    ) %x%
      splines2::bSpline(y$latitude,
        knots = innerKnots$latitude,
        degree = degree, intercept = TRUE
      )
    A_ww <- splines2::bSpline(y$longitude,
      knots = innerKnots$longitude,
      degree = degree, intercept = TRUE
    ) %x%
      splines2::dbs(y$latitude,
        knots = innerKnots$latitude, derivs = 2,
        degree = degree, intercept = TRUE
      )
    A_vw <- splines2::dbs(y$longitude,
      knots = innerKnots$longitude, derivs = 1,
      degree = degree, intercept = TRUE
    ) %x%
      splines2::dbs(y$latitude,
        knots = innerKnots$latitude, derivs = 1,
        degree = degree, intercept = TRUE
      )

    A_vv <- A_vv * inside
    A_ww <- A_ww * inside
    A_vw <- A_vw * inside

    A <- list(
      A_vv = A_vv,
      A_ww = A_ww,
      A_vw = A_vw
    )

    A_sqrt <- t(A_vv) %*% A_vv + t(A_ww) %*% A_ww + 2 * (t(A_vw) %*% A_vw)
  }

  message("Initializing spline.")

  rawSpline <- initSpline(
    y = y, knots = innerKnots, degree = degree,
    intercept = TRUE, dim
  )

  message("Setting penalty function.")

  penalize <- pen(beta,
    rawSpline = rawSpline, m = m,
    b = b, lambda = lambda, split = split, A = A_sqrt,
    prop = prop, dim = dim, res = res,
    inside = inside, normalize = sum(inside, na.rm = TRUE)
  )

  gradient <- gr(beta,
    rawSpline = rawSpline, m = m,
    lambda = lambda, split = split, A = A,
    prop = prop, dim = dim, res = res,
    inside = inside, normalize = sum(inside, na.rm = TRUE)
  )

  message("Setting gradient function.")


  return(new_optimizationObject(
    markRecaptureObject = markRecaptureObject,
    initBeta = initBeta,
    y = y,
    knots = knots,
    b = b,
    degree = degree,
    lambda = lambda,
    split = split,
    penalize = penalize,
    gradient = gradient,
    rawSpline = rawSpline,
    inside = inside
  ))
}
