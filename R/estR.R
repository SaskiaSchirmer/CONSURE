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

#' recovery probability estimator
#'
#' This function estimates the recovery probability uniformly over the whole
#' recovery area.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param b name of breeding area to estimate recovery for. Defaults to "all".
#' @return scalar of recovery probability
#' @export
#' @examples mro <- estR(mro1D)
estR <- function(markRecaptureObject, b = "all") {
  k <- markRecaptureObject$breedingAreas[[b]]$numberOfRecoveries
  c <- markRecaptureObject$estimates$c[[b]]
  n <- markRecaptureObject$breedingAreas[[b]]$markedInds

  markRecaptureObject$estimates[["r"]] <- c * k / n
  markRecaptureObject
}
