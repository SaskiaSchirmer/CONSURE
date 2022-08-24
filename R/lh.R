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

#' function defining the smoothness of a bspline
#'
#' This function defines the smoothness of a bspline as the second derivative
#' @param beta vector of parameters of the b-spline
#' @param dim dimension of space
#' @param A matrix of second derivative of b-spline
#' @param normalize factor to normalize with.
#'
#' @return function defining the distance between a b-spline and
#'         discrete migratory connectivity depending on the parameters
#' @export
#' @examples{
#'     y <- seq(0,1,length.out=100)
#'     i_k <- seq(0.1111111,0.8888889,length.out=8)
#'      A <- splines2::dbs(y, knots=i_k, derivs = 2, degree = 3,
#'                         intercept = TRUE)
#'     L <- lh(beta, dim = 1,
#'         A = t(A) %*% A,
#'         normalize = 100)
#'     L(beta = rnorm(12))
#' }
lh <- function(beta, dim, A, normalize) {
  func <- function(beta) {
    t(beta) %*% A %*% beta / normalize
  }

  return(func)
}
