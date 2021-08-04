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

#' subdensity function for not seen individuals
#'
#' This function sets up the subdensity function for every spatio-temporal point
#' for not seen individuals by means of given survival, migratory connectivity
#' and recovery probability. Not seen individuals died in observation time but
#' were not found or survived observation time.
#' @param w decimal number (1D) or vector of decimal numbers (2D): spatial point
#' of recovery
#' @inheritParams p_nf
#' @return subdensity of not seen individuals for the specified parameters
#' @export
#' @examples fnfs <- f_nf_sub(1, 1, mro1D)
f_nf_sub <- function(w, b, markRecaptureObject) {
  r <- markRecaptureObject$winteringArea$recovery
  s <- markRecaptureObject$winteringArea$survival
  m <- markRecaptureObject$breedingAreas[[b]]$migratoryConnectivity
  oT <- markRecaptureObject$observationTime

  ((1 - r(w)) * (1 - s(w)^oT) + s(w)^oT) * m(w)
}
