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

#' plot 3D plot using plotly
#'
#' This function plots the kernel density estimate and true density for
#' simulated data.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param param parameter to be plotted. Can be "m" or "s". Only one parameter
#' can be plotted at once. Select different areas of origin with the argument
#' b.
#' @param b specifies breeding area for which the plot is drawn. Can be either
#' a breedingAreaName, the corresponding number of the breeding area or "all"
#' for all breeding areas at once.
#' @param filename optional filename extension. Defaults to "".
#' @param noCI suppresses drawing the confidence interval, even if bootstrap
#' information is in the markRecaptureObject. Defaults to FALSE.
#' @importFrom dplyr %>%
#' @return saves a html file and returns the plotly plot
#' @export
#' @examples plotM(mro1D, trueValuesAvailable = TRUE)

plotlyParam <- function(markRecaptureObject, param, b = "all",
                        filename = "", noCI = FALSE){

  est <- markRecaptureObject$estimates[[param]]

  if(param == "m"){
    est <- est[[b]]
  }

  bootstrap <- markRecaptureObject$estimates$bootstrap$bootstrapQuantiles %>%
    dplyr::filter(parameter == param,
                  markArea == b)
  if(!noCI & !is.null(bootstrap)){

  upperBootstrapQuantile <- bootstrap %>%
    dplyr::select(longitude, latitude, uq) %>%
    tidyr::pivot_wider(names_from = latitude, values_from = uq)

  upperBootstrapQuantile <- t(
    upperBootstrapQuantile[,3:ncol(upperBootstrapQuantile)]
  )

  lowerBootstrapQuantile <- bootstrap %>%
    dplyr::select(longitude, latitude, lq) %>%
    tidyr::pivot_wider(names_from = latitude, values_from = lq)

  lowerBootstrapQuantile <- t(
    lowerBootstrapQuantile[,3:ncol(lowerBootstrapQuantile)]
  )
  }

  param_fig <- plotly::plot_ly(showscale = FALSE) %>%
    plotly::add_surface(z = ~est) %>%
    plotly::layout(scene = list(xaxis = list(title = "longitude"),
                                yaxis = list(title = "latitude"),
                                zaxis = list(title = param)))

  if(!noCI & !is.null(bootstrap)){
    param_fig <- param_fig %>%
      plotly::add_surface(z = ~upperBootstrapQuantile,  opacity = 0.5,
                          colorscale = list(c(0, 1), c("grey", "grey"))) %>%
      plotly::add_surface(z = ~lowerBootstrapQuantile, opacity = 0.5,
                          colorscale = list(c(0, 1), c("grey", "grey")))
  }

  htmlwidgets::saveWidget(param_fig, paste(param, "_",filename,".html", sep = ""),
             selfcontained = T)

  param_fig
}
