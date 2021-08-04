# CONSURE - Continuous Survival, Use of Space and Recovery Probability Estimates.
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

#' simulation function for mark-recovery setting
#'
#' This function allows you to simulate data of dead recoveries with specified
#' survival, migratory connectivity and recovery probability in 1D- or 2D-space
#' and time using rejection sampling.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @return returns object of class markRecaptureObject with added simulated
#' recoveryData
#'
#' @export
#' @examples {
#'  mro <- markRecaptureObject(xrange = c(0,1),
#'                             survival = function(w){0.5*w+.4},
#'                             recovery = function(w){0.01},
#'                             markedInds = rep(1000,3),
#'                             migratoryConnectivity = function(b,w,B=3){
#'                               truncnorm::dtruncnorm(w,0,1,
#'                               mean = seq(0.1,0.9,length.out = B)[b],
#'                               sd = 0.3)},
#'                            observationTime = 5)
#' mro <- simContin(mro)
#' }

simContin <- function(markRecaptureObject) {
  eta <- list()
  k <- numeric()
  oT <- markRecaptureObject$observationTime
  breedingAreaNames <- names(markRecaptureObject$breedingAreas)[
    names(markRecaptureObject$breedingAreas) != "all"
  ]

  for (b in breedingAreaNames) {
    # calculate probability to be not seen independent of space and time for
    # every breeding area
    p <- 1 - p_nf(b, markRecaptureObject)
    # 1st step: simulate count of found individuals
    k[b] <- stats::rbinom(
      1, markRecaptureObject$breedingAreas[[b]]$markedInds,
      p
    )

    # 2nd step: sample data from subdensity
    # using rejection sampling

    if (markRecaptureObject$spatialDim != 1) {
      f_f2 <- function(x) {
        f_f(w = c(x[1], x[2]), t = x[3], b = b, markRecaptureObject, p)
      }

      dg <- function(x) {
        prod(c(
          stats::dbeta(x[1], shape1 = 1, shape2 = 2),
          stats::dbeta(x[2], shape1 = 1, shape2 = 2),
          truncdist::dtrunc(x[3], "geom", 0, oT, prob = 0.2)
        )) +
          0.0000000001
      }
      rg <- function(n) {
        c(
          stats::rbeta(n, shape1 = 1, shape2 = 2),
          stats::rbeta(n, shape1 = 1, shape2 = 2),
          truncdist::rtrunc(n, "geom", 0, oT, prob = 0.2)
        )
      }
      cnames <- c("markArea", "longitude", "latitude", "age")
    } else {
      f_f2 <- function(x) {
        f_f(w = x[1], t = x[2], b = b, markRecaptureObject, p)
      }

      dg <- function(x) {
        prod(c(
          stats::dbeta(x[1], shape1 = 1, shape2 = 2),
          truncdist::dtrunc(x[2], "geom", 0, oT, prob = 0.2)
        )) +
          0.0000000001
      }
      rg <- function(n) {
        c(
          stats::rbeta(n, shape1 = 1, shape2 = 2),
          truncdist::rtrunc(n, "geom", 0, oT, prob = 0.2)
        )
      }

      cnames <- c("markArea", "longitude", "age")
    }

    eta[[b]] <- SimDesign::rejectionSampling(k[b] + 1,
      df = f_f2, dg = dg,
      rg = rg, M = 10
    )
    eta[[b]] <- as.data.frame(eta[[b]][-nrow(eta[[b]]), ])
    eta[[b]] <- cbind(b, eta[[b]], stringsAsFactors = FALSE)
    colnames(eta[[b]]) <- cnames

    markRecaptureObject$breedingAreas[[b]]$numberOfRecoveries <- unname(k[b])
  }
  markRecaptureObject$winteringArea$recoveryData <- eta

  markRecaptureObject$breedingAreas[["all"]]$numberOfRecoveries <- sum(k)

  return(markRecaptureObject)
}
