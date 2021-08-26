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

#' gradient of function defining the smoothness of a bspline
#'
#' This function defines the smoothness of a bspline as the second derivative
#' @param beta vector of parameters of the b-spline
#' @param k number of parameters
#' @param dim dimension of space
#' @param A matrix of second derivative of b-spline
#' @param normalize numeric, normalizes the discretized integralt. Equals to the
#' spatial resolution in one-dimensional space and to the product of the spatial
#' resolutions in two-dimensional space.
#'
#' @return function defining the distance between a b-spline and discrete
#'         migratory connectivity depending on the parameters
#' @export
#' @examples{
#'     y <- seq(0,1,length.out=100)
#'     iK <- seq(0.1111111,0.8888889,length.out=8)
#'     grL <- grLh(beta,k,dim = 1,
#'         A = splines2::dbs(y,knots=iK,derivs = 2, degree = 3,
#'                           intercept = TRUE),
#'         normalize = 100)
#'     grL(beta = rnorm(12), k = 2)
#' }

grLh <- function(beta, k, dim, A, normalize) {
  func <- function(beta, k) {
    if (dim == 1) {
      A_k <- sum(A %*% beta * A[, k])
    } else if (dim == 2) {
      A_k <- sum(A$A_vv %*% beta * A$A_vv[, k] +
        2 * A$A_vw %*% beta * A$A_vw[, k] +
        A$A_ww %*% beta * A$A_ww[, k])
    } else {
      message("grLh: Check number of dimensions.")
    }
    2 * A_k / normalize
  }

  return(func)
}
