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

#' function to define a positive a b-spline function depending on its parameters
#'
#' This function defines the exponential of a b-spline dependent on its
#' parameters beta
#' @param raw_spline raw spline, value of init_spline()
#' @param beta parameter of the b-spline, values are to be optimized
#' @param inside specifies if a cell of the gridded window is inside the window
#'               of the data or not. Vector of logicals.
#'
#' @return function defining a b-spline for a given parameter vector
#' @export
#' @examples{
#'     y <- seq(0,1,length.out=100)
#'     i_k <- seq(0.1111111,0.8888889,length.out=8)
#'     r_s <- initSpline(y = y,
#'         knots = i_k,
#'         degree = 3,
#'         intercept = TRUE,
#'         dim = 1)
#'     d_b <- define_bspline(raw_spline = r_s,
#'         beta,
#'         inside = rep(TRUE, 100))
#'     d_b(beta = rnorm(12))
#' }


define_bspline <- function(raw_spline, beta, inside) {
  print("defBspline")

  return(
    function(beta) {
      exp(raw_spline %*% beta) * inside
    }
  )
}
