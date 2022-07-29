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

#' estimate migratory connectivity
#'
#' This function estimates the migratory connectivity in space constant over
#' time when survival and raw distribution of dead recoveries for every breeding
#' area is known.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param all boolean: if TRUE only one kernel density estimate will be
#' calculated summarizing all breeding areas. Defaults to FALSE.
#' @return list of vectors with length res-1 containing migratory connectivity
#' density of every spot
#' @export
#' @examples mro <- estM(mro1D, all = TRUE)
estM <- function(markRecaptureObject, all = FALSE) {
  s_fit <- markRecaptureObject$estimates$s
  breedingAreaNames <- names(markRecaptureObject$breedingAreas)[
    !grepl("all", names((markRecaptureObject$breedingAreas)))
  ]
  dim <- markRecaptureObject$spatialDim
  win <- markRecaptureObject$winteringArea$window
#
#   if (dim == 1) {
#     normalize <- sum(markRecaptureObject$inside[1, ], na.rm = TRUE)
#   } else if (dim == 2) {
#     normalize <- sum(markRecaptureObject$inside, na.rm = TRUE)
#   } else {
#     message("Wrong dimension!")
#   }

  if (dim == 1) {
    normalize <- markRecaptureObject$winteringArea$window$xrange[2]-
      markRecaptureObject$winteringArea$window$xrange[1]
  } else if (dim == 2) {
    normalize <- spatstat.geom::area(win)
  }

  if (all) {
    lm <- markRecaptureObject$estimates$lm$all
    markRecaptureObject$estimates[["m"]][["all"]] <-
      exp(lm$intercept - log(1 - s_fit))
    if (dim == 1) {
      markRecaptureObject$estimates[["c"]]["all"] <-
        sum(markRecaptureObject$estimates$m$all)  /
        sum(colSums(markRecaptureObject$inside) > 0)#/ normalize
      markRecaptureObject$estimates[["m"]][["all"]] <-
        markRecaptureObject$estimates[["m"]][["all"]] /
          markRecaptureObject$estimates[["c"]]["all"] *
          (colSums(markRecaptureObject$inside) > 0)
    } else if (dim == 2) {
      markRecaptureObject$estimates[["c"]]["all"] <-
        sum(markRecaptureObject$estimates[["m"]][["all"]], na.rm = TRUE) /
        sum(markRecaptureObject$inside)
      markRecaptureObject$estimates[["m"]][["all"]] <-
        markRecaptureObject$estimates[["m"]][["all"]] /
          markRecaptureObject$estimates[["c"]]["all"] *
          markRecaptureObject$inside
    }
  } else {
    for (b in breedingAreaNames) {
      markRecaptureObject <- estLM(markRecaptureObject,
        b = b,
        fixedSlope = markRecaptureObject$estimates$s
      )
      lm <- markRecaptureObject$estimates$lm[[b]]
      markRecaptureObject$estimates[["m"]][[b]] <-
        exp(lm$intercept - log(1 - s_fit))
      if (dim == 1) {
        markRecaptureObject$estimates[["c"]][b] <-
          sum(markRecaptureObject$estimates[["m"]][[b]]) /
          sum(colSums(markRecaptureObject$inside) > 0)#/ normalize
        markRecaptureObject$estimates[["m"]][[b]] <-
          markRecaptureObject$estimates[["m"]][[b]] /
            markRecaptureObject$estimates[["c"]][b] *
            (colSums(markRecaptureObject$inside) > 0)
      } else if (dim == 2) {
        markRecaptureObject$estimates[["c"]][b] <-
          sum(markRecaptureObject$estimates[["m"]][[b]], na.rm = TRUE) /
          sum(markRecaptureObject$inside)
        markRecaptureObject$estimates[["m"]][[b]] <-
          markRecaptureObject$estimates[["m"]][[b]] /
            markRecaptureObject$estimates[["c"]][b] *
            markRecaptureObject$inside
      }
    }
  }

  return(markRecaptureObject)
}
