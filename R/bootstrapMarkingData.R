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


#' This function bootstraps the marking data for every area of origin.
#' @inheritParams estS
#' @return  markRecaptureObject with estimates of all selected parameters
#' @export
#' @examples mro <- bootstrapMarkingData(mro1D)

bootstrapMarkingData <- function(markRecaptureObject) {

  breedingAreaNames <- names(markRecaptureObject$winteringArea$recoveryData)
  ls <- as.list(breedingAreaNames[breedingAreaNames != "all"])

  out <- lapply(ls, FUN = function(x){

    vec2_b1 <- sample(1:markRecaptureObject$breedingAreas[[x]]$markedInds,
                      replace = TRUE)
    vec2_b1_rec <- vec2_b1[vec2_b1 <= markRecaptureObject$breedingAreas[[x]]$numberOfRecoveries]

    list(markArea = x,
         realRecoveries = markRecaptureObject$winteringArea$recoveryData[[x]][vec2_b1_rec,],
         markedInds = length(vec2_b1[vec2_b1 > markRecaptureObject$breedingAreas[[x]]$numberOfRecoveries])+length(vec2_b1_rec))
    })


  realRecoveries <- do.call("rbind",lapply(out, function(x) x$realRecoveries))
  markedInds <- sapply(out, function(x) x$markedInds)

  bootstrap_markRecaptureObject <- markRecaptureObject(window = markRecaptureObject$winteringArea$window,
                                                      realRecoveries = realRecoveries,
                                                      markedInds = markedInds,
                                                      observationTime = markRecaptureObject$observationTime,
                                                      breedingAreaNames =  names(markRecaptureObject$winteringArea$recoveryData))

  bootstrap_markRecaptureObject$spatialResolution <- markRecaptureObject$spatialResolution


  return(bootstrap_markRecaptureObject)
}
