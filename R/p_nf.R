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
#' @param b specifies the area of origin for which the plot is drawn. Can be
#' either a origin_name, the corresponding number of the area of origin or "all"
#' for all areas of origin at once.
#' @inheritParams sim_contin
#' @return list: probability to be not seen independent of space
#' and time for every area of origin
#' @export
#' @examples p_nf(b = "b1", mro1D)
#'
p_nf <- function(b, mark_recapture_object) {
  p_nf <- numeric()
  xrange <- mark_recapture_object$destination$window$xrange
  yrange <- mark_recapture_object$destination$window$yrange

  if (identical(xrange, c(0, 0))) {
    lb <- yrange[1]
    ub <- yrange[2]
  } else if (identical(yrange, c(0, 0))) {
    lb <- xrange[1]
    ub <- xrange[2]
  } else {
    lb <- c(xrange[1], yrange[1])
    ub <- c(xrange[2], yrange[2])
  }
  p_nf <- cubature::adaptIntegrate(
    f = f_nf_sub,
    lower = lb, upper = ub, b = b,
    mark_recapture_object = mark_recapture_object
  )$integral
  return(p_nf)
}
