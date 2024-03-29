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

#' projects mark_recapture_object to another coordinate system
#'
#' This function projects all relevant components of a mark_recapture_object to
#' another coordinate system, which can be specified. These components are
#' the recovery data and the window.
#'
#' @param mark_recapture_object object of class mark_recapture_object
#' (see mark_recapture_object())
#' @param lon column name of the column containing information on longitude
#' @param lat column name of the column containing information on latitude
#' @param old_crs coordinate system of the input owin-object. Defaults to
#' longitude/latitude "EPSG:4326"
#' @param new_crs coordinate system of the output owin-object. Defaults to the
#' Mollweide Projection "ESRI:54009"
#' @return data.frame with projected coordinate-system
#' @export
#' @examples mro2 <- project_mark_recapture(mro2D)
#'
project_mark_recapture <- function(mark_recapture_object,
                                   lon = "longitude",
                                   lat = "latitude",
                                   old_crs = NULL,
                                   new_crs = "ESRI:54009") {
  if (is.null(old_crs)) {
    crs <- mark_recapture_object$destination$crs
  }

  if (crs != new_crs) {
    mark_recapture_object$destination$recovery_data <- lapply(
      mark_recapture_object$destination$recovery_data,
      function(x) project_df(x, lon = lon, lat = lat)
    )

    mark_recapture_object$destination$window <- project_window(
      mark_recapture_object$destination$window
    )
  }


  mark_recapture_object
}
