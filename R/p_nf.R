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

#' overall probability to be not seen
#'
#' This function integrates the subdensity of not seen individuals.
#' @param b specifies breeding area for which the plot is drawn. Can be either
#' a breedingAreaName, the corresponding number of the breeding area or "all"
#' for all breeding areas at once.
#' @inheritParams simContin
#' @return list: probability to be not seen independent of space
#' and time for every breeding area
#' @export
#' @examples p_nf(b = "b1", mro1D)
p_nf <- function(b, markRecaptureObject) {
  p_nf <- numeric()
  if (identical(markRecaptureObject$winteringArea$window$xrange, c(0, 0))) {
    lb <- markRecaptureObject$winteringArea$window$yrange[1]
    ub <- markRecaptureObject$winteringArea$window$yrange[2]
  } else if (identical(
    markRecaptureObject$winteringArea$window$yrange,
    c(0, 0)
  )) {
    lb <- markRecaptureObject$winteringArea$window$xrange[1]
    ub <- markRecaptureObject$winteringArea$window$xrange[2]
  } else {
    lb <- c(
      markRecaptureObject$winteringArea$window$xrange[1],
      markRecaptureObject$winteringArea$window$yrange[1]
    )
    ub <- c(
      markRecaptureObject$winteringArea$window$xrange[2],
      markRecaptureObject$winteringArea$window$yrange[2]
    )
  }
  p_nf <- cubature::adaptIntegrate(
    f = f_nf_sub,
    lower = lb, upper = ub, b = b,
    markRecaptureObject = markRecaptureObject
  )$integral
  return(p_nf)
}
