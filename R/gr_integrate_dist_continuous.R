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

#' gradient of function to define the distance between a b-spline representing a
#' density and the continuous migratory connectivity estimate
#'
#' This function numerically integrates the quadratic distance between a
#' b-spline representing a density and the continuous migratory connectivity
#' estimate.
#' @inheritParams defineBspline
#' @param k number of parameters
#' @param dim spatial dimension of the data
#' @param m vector of continuous migratory connectivity estimate
#' @param inside specifies if a cell of the gridded window is inside the window
#'               of the data or not. Vector of logicals.
#' @param normalize numeric, normalizes the discretized integralt. Equals to the
#'                  spatial resolution in one-dimensional space and to the
#'                  product of the spatial resolutions in two-dimensional space.
#'
#' @return function defining the distance between a b-spline and continuous
#'         migratory connectivity
#' @export
#' @examples{
#'     y <- seq(0,1,length.out=100)
#'     iK <- seq(0.1111111,0.8888889,length.out=8)
#'     rS <- initSpline(y=y,
#'         knots = iK,
#'         degree = 3,
#'         intercept = TRUE,
#'         dim = 1)
#'     grIC <- grIntegrateDist2Continuous(beta,k,
#'         rawSpline = rS,
#'         dim = 1,
#'         m = mro1DIncreasing$mro$estimates$m$all,
#'         inside = rep(1,100),
#'         normalize = 100)
#'    grIC(beta = rnorm(12), k = 2)
#' }

grIntegrateDist2Continuous <- function(beta, k, rawSpline, dim, m, inside,
                                       normalize) {
  print(paste("intCon", dim))


  bspline <- defineBspline(rawSpline = rawSpline, beta = beta, inside = inside)

  dh <- function(beta, k) {
    dhsum <- numeric()
    b <- bspline(beta)
    for (j in seq_len(length(inside))) {
      dhsum[j] <- sum((rawSpline[j, k] - rawSpline[, k]) * b)
    }

    return(dhsum)
  }

  if (sum(is.nan(m)) != 0) message("matrix of continuous m contains NaN values")



  function(beta, k) {
    return(
      2 / sum(bspline(beta))^2 *
        sum(
          (
            bspline(beta) / sum(bspline(beta)) * normalize - c(m)
          ) * bspline(beta) * dh(beta, k),
          na.rm = TRUE
        )
    )
  }
}
