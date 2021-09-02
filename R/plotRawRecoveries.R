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

#' plot raw recoveries of real world data
#'
#' This function plots the distribution of recoveries in the real world data in
#' Europe and Africa.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param pdfName string to name pdf-file, please include ".pdf"!
#' @param areaNames optional, order of breeding area names to be plotted,
#' defaults to NULL.
#' In the default case order of breeding areas in markRecaptureObject will be
#' chosen.
#' @param facetByAge logical, determines if the data should be faceted by the
#' age column. Defaults to FALSE.
#' @param facetByArea logical, determines if the data should be faceted by
#' the area column.
#' Defaults to FALSE.
#' @param ageMin numeric. Defaults to 0. If set only data points older than
#' ageMin are plotted.
#' @param ageMax numeric. If set only data points younger or equal ageMax are
#' plotted. Defaults to NULL.
#' @param xname character, name of column containing first dimension, defaults
#' to "longitude".
#' @param yname character, name of column containing second dimension, defaults
#' to "latitude".
#' @param timename character, name of column containing third dimension,
#' defaults to "age".
#' @param markAreaName character, name of column containing information on
#' areas where marking took place, defaults to "markArea".
#' @param plotTitle character, title of plot, defaults to "".
#' @param map map to plot the data on. Needs to be compatible with ggplot2.
#'            For example, map can be ggmap::ggmap(myMap), where myMap is
#'            created by ggmap::get_stamenmap.
#' @param prj projection of the coordinates of the dead recoveries. Defaults to
#'        "+proj=longlat". For details see ?sf::st_crs. If the data has another
#'        projection it will be transformed to the longitude latitude system
#'        using sf::st_transform
#' @param transformToPrj projection the dead recoveries are transformed to.
#'        Defaults to "+proj=longlat". Should be the projection of the map.
#'        Data will only be transformed when transformToPrj does not equal prj.
#'
#' @return depending on arguments plot as pdf or to plot to device
#' @export
#' @examples plotRawRecoveries(mro1D)
plotRawRecoveries <- function(markRecaptureObject, pdf = FALSE,
                              pdfName = "rawRecoveries.pdf",
                              areaNames = NULL, facetByAge = FALSE,
                              facetByArea = FALSE,
                              ageMin = 0, ageMax = NULL,
                              xname = "longitude", yname = "latitude",
                              timename = "age", markAreaName = "markArea",
                              plotTitle = "", map = NULL,
                              prj = "+proj=longlat",
                              transformToPrj = "+proj=longlat") {
  dim <- markRecaptureObject$spatialDim

  if (pdf) pdf(pdfName)

  if (dim == 1) {
    dat <- do.call("rbind", markRecaptureObject$winteringArea$recoveryData)

    pl <- ggplot2::ggplot(ggplot2::aes_string(xname, timename),
                          data = dat
    ) +
      ggplot2::labs(x = "non-breeding area", y = "age", title = plotTitle) +
      ggplot2::geom_point(shape = 3)

    if (facetByArea) {
      pl <- pl +
        ggplot2::facet_grid(stats::reformulate(".", markAreaName))
    }
    if (!facetByAge) message("Not facetting by age creates no meaningful plot.
                            Facetting by age anyways.")
  } else if (dim == 2) {
    if (is.null(areaNames)) {
      areaNames <- names(markRecaptureObject$winteringArea$recoveryData)
    }

    dat <- do.call("rbind",markRecaptureObject$winteringArea$recoveryData)

    if(prj != transformToPrj){
      tmp <- sf::st_as_sf(x = dat,
                          coords = c(xname, yname),
                          crs = prj)
      tmp = sf::st_transform(tmp, crs = transformToPrj)
      dat[,c(xname,yname)] <- data.frame(sf::st_coordinates(tmp))

    }

    if (is.null(ageMax)) {
      ageMax <- max(dat[timename])
    }

    dat <- dat[dat[timename] > ageMin & dat[timename] <= ageMax &
                 as.character(unlist(dat[markAreaName])) %in% areaNames, ]
    dat[markAreaName] <- factor(unlist(dat[markAreaName]), levels = areaNames)

    colnames(dat)[colnames(dat) == xname] <- "lon"
    colnames(dat)[colnames(dat) == yname] <- "lat"

    if(is.null(map)){
      pl <- ggplot2::ggplot()
    } else{
      pl <- map
    }
    pl <- pl+
      ggplot2::geom_point(data = dat, ggplot2::aes(x = .data$lon, y = .data$lat)) +
      ggplot2::labs(x = "longitude", y = "latitude", title = plotTitle) +
      ggplot2::theme(text = ggplot2::element_text(size = 24))
    if (facetByAge) {
      if (facetByArea) {
        pl <- pl +
          ggplot2::facet_grid(stats::reformulate(timename, markAreaName))
      } else {
        pl <- pl + ggplot2::facet_grid(stats::reformulate(timename, "."))
      }
    } else if (facetByArea) {
      pl <- pl +
        ggplot2::facet_grid(stats::reformulate(".", markAreaName))
    }
  }

  if (pdf) {
    plot(pl)
    grDevices::dev.off()
  }
  pl
}
