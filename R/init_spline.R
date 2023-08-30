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

#' Function to initialize a b-spline for optimization.
#'
#' This function initilizes a b-spline on a helper sequence to be used for
#' optimization.
#' @param y numeric vector, helper sequence, has to span whole wintering area,
#' defaults to seq(0,1,length.out = 100)
#' @param knots numeric vector, inner knot sequence to span the b-spline
#' @param degree integer, degree of the piecewise polynomial the b-spline is
#' made of, defaults to 3 (cubic b-spline)
#' @param intercept logical, defines if intercept is estimated or not, defaults
#' to TRUE, do not change default without good reason!
#' @param dim spatial dimension. Integer. Can be 1 or 2.
#' @return returns a bSpline. See ?splines2::bSpline for details.
#' @export
#'
#' @examples{
#'     y <- seq(0, 1, length.out = 100)
#'     i_k <- seq(0.1111111, 0.8888889, length.out = 8)
#'     i_s <- init_spline(y = y, knots = i_k, degree = 3, dim = 1)
#' }

init_spline <- function(y, knots, degree, intercept = TRUE, dim) {
  print("startInitSpline")
  print(knots)
  if (dim == 1) {
    bspline <- splines2::bSpline(y,
      knots = knots, degree = degree,
      intercept = intercept
    )
  } else if (dim == 2) {
    bspline <- splines2::bSpline(y$longitude,
      knots = knots$longitude,
      degree = degree, intercept = TRUE
    ) %x%
      splines2::bSpline(y$latitude,
        knots = knots$latitude, degree = degree,
        intercept = TRUE
      )
  }
  return(bspline)
}
