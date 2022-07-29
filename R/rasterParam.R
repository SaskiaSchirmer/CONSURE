# CONSURE - Continuous Survival, Use of Space and Recovery Probability Estimates.
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
#' @inheritParams estS
#' @param param name of the parameter
#' @param b name of area of origin
#' @importFrom dplyr %>%
#'
#' @return raster stack of parameter value and bootstrap confidence interval.
#' @export
#' @examples rasterParam(mro2D, "s", "all")


  rasterParam <- function(markRecaptureObject, param, b){

    res <- markRecaptureObject$spatialResolution
    win <- markRecaptureObject$winteringArea$window
    est <- markRecaptureObject$estimates[[param]]
    bootstrap <- markRecaptureObject$estimates$bootstrap$bootstrapQuantiles

    if(param == "m"){
      est <- est %>% dplyr::filter(markArea == "b")
    }
    est <- est[res:1,]

    if(!is.null(bootstrap)){
      bootstrap <- bootstrap %>%
        dplyr::arrange(dplyr::desc(latitude)) %>%
        dplyr::filter(parameter == param,
               markArea == b)
    }


    myRaster_pre <- function(res, win, val){
      function(val){
        terra::rast(nrows = res, ncols = res,
                    xmin = win$xrange[1],
                    xmax = win$xrange[2],
                    ymin = win$yrange[1],
                    ymax = win$yrange[2],
                    vals = val)
      }
    }

    myRaster <- myRaster_pre(res, win, val)

    rast_param <- myRaster(est)

    if(!is.null(bootstrap)){
      rast_bootstrap_uq <- myRaster(bootstrap$uq)
      rast_bootstrap_lq <- myRaster(bootstrap$lq)
      rasteredParam <- c(rast_param, rast_bootstrap_uq, rast_bootstrap_lq)
    } else{
      rasteredParam <- rast_param
    }


    return(rasteredParam)
  }
