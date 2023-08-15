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

#' gradient function to define the distance between a b-spline representing a
#' density and the discrete proportion
#'
#' This function numerically integrates the quadratic distance between a
#' b-spline representing a density and the discrete migratory connectivity
#' @inheritParams define_bspline
#' @param k number of parameters
#' @param dim spatial dimension of the data
#' @param split vector of length of y which defines the affiliation to a
#'              discrete wintering area
#' @param prop vector of proportions of individuals going to discrete wintering
#'             areas (discrete estimate or expected value for migratory
#'             connectivity)
#' @param inside specifies if a cell of the gridded window is inside the window
#'               of the data or not. Vector of logicals.
#'
#' @return function defining the distance between a b-spline and
#'         discrete migratory connectivity depending on the parameters
#' @export
#' @examples{
#'     y <- seq(0,1,length.out=100)
#'     i_k <- seq(0.1111111,0.8888889,length.out=8)
#'     r_s <- init_spline(y=y,
#'         knots = i_k,
#'         degree = 3,
#'         intercept = TRUE,
#'         dim = 1)
#'     gr_id <- gr_integrate_dist_discrete(beta, k,
#'         raw_spline = r_s,
#'         dim = 1,
#'         split = mro1D_increasing$split,
#'         prop = mro1D_increasing$mro$origins$all$m_discrete /
#'              sum(mro1D_increasing$mro$origins$all$m_discrete),
#'         inside = rep(1, 100))
#'    gr_id(beta = rnorm(12), k = 2)
#' }

gr_integrate_dist_discrete <- function(beta, k, raw_spline, dim,
                                     split,
                                     prop,
                                     inside) {
  print("intDisc")


  bspline <- define_bspline(raw_spline = raw_spline, beta = beta,
                            inside = inside)



  return(
    function(beta, k) {
      ls_raw_spline <- list()
      ls_bspline <- list()

      for (i in unique(split[!is.na(split)])) {
        ls_raw_spline[[i]] <- raw_spline[!is.na(split) & split == i, ]
        ls_bspline[[i]] <- bspline(beta)[!is.na(split) & split == i, ]
      }

      return(sum(2 *
        (sapply(
          ls_bspline,
          function(x) sum(x) / sum(bspline(beta))
        ) -
          as.numeric(prop)) *
        (sapply(
          Map(
            "*",
            lapply(ls_raw_spline, function(x) x[, k]),
            ls_bspline
          ),
          sum
        ) *
          sum(bspline(beta)) -
          sapply(ls_bspline, sum) *
            sum(raw_spline[, k] * bspline(beta))
        ) / sum(bspline(beta))^2 / as.numeric(prop)))
    }
  )
}
