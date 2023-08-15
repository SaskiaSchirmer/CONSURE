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

#' function to define the distance between a b-spline representing a density
#' and the continuous migratory connectivity estimate
#'
#' This function numerically integrates the quadratic distance between a
#' b-spline representing a density and the continuous migratory connectivity
#' estimate
#' @inheritParams define_bspline
#' @param m vector of continuous migratory connectivity estimate
#' @param inside specifies if a cell of the gridded window is inside the window
#'               of the data or not. Vector of logicals.
#' @param normalize numeric, normalizes the discretized integralt. Equals to the
#'                  spatial resolution in one-dimensional space and to the
#'                  product of the spatial resolutions in two-dimensional space.
#'
#' @return function defining the distance between a b-spline and
#'         continuous migratory connectivity
#' @export
#' @examples{
#'     y <- seq(0,100222.75,length.out=100)
#'     i_k <- seq(0,100222.75,length.out=10)[2:9]
#'     r_s <- init_spline(y = y,
#'         knots = i_k,
#'         degree = 3,
#'         intercept = TRUE,
#'         dim = 1)
#'     i_c <- integrate_dist_continuous(raw_spline = r_s, beta,
#'            m = mro1D_increasing$mro$estimates$m$all,
#'            inside = rep(TRUE, 100))
#'     i_c(rnorm(12))
#' }

integrate_dist_continuous <- function(raw_spline, beta, m, inside, normalize) {
  print(paste("intCon"))



  bspline <- define_bspline(
    raw_spline = raw_spline, beta = beta,
    inside = inside
  )


  if (sum(is.nan(m)) != 0) message("matrix of continuous m contains NaN values")

  function(beta) {
    return(
      sum(
        (
          bspline(beta) /
            sum(bspline(beta)) * normalize
            - c(m)
        )^2,
        na.rm = TRUE
      )
    )
  }
}
