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

#' function to define the distance between a b-spline representing a density and
#' the discrete proportion
#'
#' This function numerically integrates the quadratic distance between a
#' b-spline representing a density and the discrete migratory connectivity
#' @inheritParams define_bspline
#' @param dim spatial dimension of the data
#' @param split vector of length of y which defines the affiliation to a
#'              discrete wintering area
#' @param b name of breeding area
#' @param prop vector of proportions of individuals going to discrete wintering
#'             areas (discrete estimate or expected value for migratory
#'             connectivity)
#' @param print logical, should proportions be printed or not?
#' @param inside specifies if a cell of the gridded window is inside the window
#'               of the data or not. Vector of logicals.
#'
#' @return function defining the distance between a b-spline and discrete
#'         migratory connectivity depending on the parameters
#' @export
#' @examples{
#'     y <- seq(0, 1, length.out=100)
#'     i_k <- seq(0.1111111, 0.8888889, length.out = 8)
#'     r_s <- init_spline(y = y,
#'         knots = i_k,
#'         degree = 3,
#'         intercept = TRUE,
#'         dim = 1)
#'     i_d <- integrate_dist_discrete(raw_spline = r_s, dim = 1,
#'         split = mro1D_increasing$split, beta, b = "all",
#'         prop = mro1D_increasing$mro$origins$all$m_discrete /
#'             sum(mro1DIncreasing$mro$origins$all$m_discrete),
#'         inside = rep(TRUE, 100))
#'     i_d(rnorm(12))
#' }

integrate_dist_discrete <- function(raw_spline, dim,
                                   split, beta,
                                   b, prop, print = TRUE,
                                   inside) {
  print("intDisc")

  def_bspline <- define_bspline(raw_spline = raw_spline, beta = beta,
                               inside = inside)

  return(
    function(beta) {
      bspline <- def_bspline(beta)

      tmp <- as.data.frame(cbind(bspline / sum(bspline), split))
      colnames(tmp) <- c("bspline", "split")

      tmp2 <- dplyr::group_by(tmp, split)
      tmp2 <- dplyr::summarise(tmp2, sum = sum(bspline))
      tmp2 <- tmp2[!is.na(tmp2$split), ]

      if (print) print(tmp2)

      if (sum(is.infinite(bspline)) > 0) {
        return(Inf)
      } else {
        return(sum(((tmp2$sum - prop) / prop)^2))
      }
    }
  )
}
