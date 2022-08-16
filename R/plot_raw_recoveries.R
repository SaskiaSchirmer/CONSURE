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
#' @param mark_recapture_object object of class mark_recapture_object
#' (see mark_recapture_object())
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param pdf_name string to name pdf-file, please include ".pdf"!
#' @param origin_names optional, order of the names of the area of origin to be
#' plotted, defaults to NULL. In the default case order of the areas of origin
#' in mark_recapture_object will be chosen.
#' @param facet_by_age logical, determines if the data should be faceted by the
#' age column. Defaults to FALSE.
#' @param facet_by_area logical, determines if the data should be faceted by
#' the area column.
#' Defaults to FALSE.
#' @param age_min numeric. Defaults to 0. If set only data points older than
#' age_min are plotted.
#' @param age_max numeric. If set only data points younger or equal age_max are
#' plotted. Defaults to NULL.
#' @param xname character, name of column containing first dimension, defaults
#' to "longitude".
#' @param yname character, name of column containing second dimension, defaults
#' to "latitude".
#' @param timename character, name of column containing third dimension,
#' defaults to "age".
#' @param mark_area_name character, name of column containing information on
#' areas where marking took place, defaults to "mark_area".
#' @param plot_title character, title of plot, defaults to "".
#' @param map map to plot the data on. Needs to be compatible with ggplot2.
#'            For example, map can be ggmap::ggmap(myMap), where myMap is
#'            created by ggmap::get_stamenmap.
#' @param prj projection of the coordinates of the dead recoveries. Defaults to
#'        "+proj=longlat". For details see ?sf::st_crs. If the data has another
#'        projection it will be transformed to the longitude latitude system
#'        using sf::st_transform
#'
#' @return depending on arguments plot as pdf or to plot to device
#' @export
#' @examples plot_raw_recoveries(mro1D)
plot_raw_recoveries <- function(mark_recapture_object, pdf = FALSE,
                                pdf_name = "raw_recoveries.pdf",
                                origin_names = NULL, facet_by_age = FALSE,
                                facet_by_area = FALSE,
                                age_min = 0, age_max = NULL,
                                xname = "longitude", yname = "latitude",
                                timename = "age", mark_area_name = "mark_area",
                                plot_title = "", map = NULL,
                                prj = "+proj=longlat") {
  dim <- mark_recapture_object$spatial_dimension

  if (pdf) pdf(pdf_name)

  if (dim == 1) {
    dat <- do.call("rbind", mark_recapture_object$destination$recovery_data)

    pl <- ggplot2::ggplot(
      ggplot2::aes_string(sf::st_coordinates(dat)[, 1], timename),
      data = dat
    ) +
      ggplot2::labs(x = "destination area", y = "age", title = plot_title) +
      ggplot2::geom_point(shape = 3)

    if (facet_by_area) {
      pl <- pl +
        ggplot2::facet_grid(stats::reformulate(".", mark_area_name))
    }
    if (!facet_by_age) message("Not facetting by age creates no meaningful plot.
                            Facetting by age despite facet_by_age = FALSE.")
  } else if (dim == 2) {
    if (is.null(origin_names)) {
      origin_names <- names(mark_recapture_object$destination$recovery_data)
    }

    dat <- mark_recapture_object$destination$recovery_data

    if (is.null(age_max)) {
      age_max <- max(sapply(dat, function(x) max(x[[timename]])))
    }

    dat <- dat[names(dat) %in% origin_names]
    dat <- lapply(dat, function(x) {
      x[x[[timename]] > age_min &
        x[[timename]] <= age_max, ]
    })

    crs <- mark_recapture_object$destination$crs
    xlim <- mark_recapture_object$destination$window$xrange
    ylim <- mark_recapture_object$destination$window$yrange

    if (rlang::is_installed("spData")) {
      pl <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = spData::world)
    } else {
      message("Install the package spData to see country boundaries.")
    }


    i <- 1
    for (b in origin_names) {
      pl <- pl +
        ggplot2::geom_sf(
          data = dat[[b]],
          colour = i, fill = NA
        )
      i <- i + 1
    }

    pl <- pl +
      ggplot2::coord_sf(
        crs = crs, expand = FALSE,
        xlim = xlim,
        ylim = ylim
      )

    pl <- pl +
      ggplot2::labs(x = "longitude", y = "latitude", title = plot_title) +
      ggplot2::theme(text = ggplot2::element_text(size = 20))

    if (facet_by_age) {
      if (facet_by_area) {
        pl <- pl +
          ggplot2::facet_grid(stats::reformulate(timename, mark_area_name))
      } else {
        pl <- pl + ggplot2::facet_grid(stats::reformulate(timename, "."))
      }
    } else if (facet_by_area) {
      pl <- pl +
        ggplot2::facet_grid(stats::reformulate(".", mark_area_name))
    }
  }

  if (pdf) {
    plot(pl)
    grDevices::dev.off()
  }
  pl
}
