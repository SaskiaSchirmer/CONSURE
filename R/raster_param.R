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

#' Raster stack of parameter values and bootstrap confidence intervals, if
#' available.
#'
#' This function creates a raster stack of the parameter value and the 2.5%-
#' and the 97.5%-quantile of the bootstrapped data.
#'
#' @inheritParams est_s
#' @param param name of the parameter
#' @param b name of area of origin
#' @importFrom dplyr %>%
#'
#' @return raster stack of parameter value and bootstrap confidence interval.
#' @export
#' @examples raster_param(mro2D, "s", "all")
#'
raster_param <- function(mark_recapture_object, param, b) {
  res <- mark_recapture_object$spatial_resolution
  win <- mark_recapture_object$destination$window
  est <- mark_recapture_object$estimates[[param]]
  bootstrap <- mark_recapture_object$estimates$bootstrap$bootstrap_quantiles

  if (param == "m") {
    est <- est %>% dplyr::filter(.data$mark_area == "b")
  }
  est <- est[res:1, ]

  if (!is.null(bootstrap)) {
    bootstrap <- bootstrap %>%
      dplyr::arrange(dplyr::desc(.data$latitude)) %>%
      dplyr::filter(
        .data$parameter == param,
        .data$mark_area == b
      )
  }


  my_raster_pre <- function(res, win, val) {
    function(val) {
      terra::rast(
        nrows = res, ncols = res,
        xmin = win$xrange[1],
        xmax = win$xrange[2],
        ymin = win$yrange[1],
        ymax = win$yrange[2],
        vals = val
      )
    }
  }

  my_raster <- my_raster_pre(res, win, val)

  rast_param <- my_raster(est)

  if (!is.null(bootstrap)) {
    rast_bootstrap_uq <- my_raster(bootstrap$uq)
    rast_bootstrap_lq <- my_raster(bootstrap$lq)
    rastered_param <- c(rast_param, rast_bootstrap_uq, rast_bootstrap_lq)
  } else {
    rastered_param <- rast_param
  }


  return(rastered_param)
}
