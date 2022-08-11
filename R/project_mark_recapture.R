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

#' projects mark_recapture_project to another coordinate system
#'
#' This function projects all relevant components of a mark_recapture_project to
#' another coordinate system, which can be specified. These components are
#' the recovery data and the window.
#'
#' @param df data.frame with columns for coordinates, e.g., longitude and
#'  latitude
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
project_mark_recapture <- function(mark_recapture_project,
                                   lon = "longitude",
                                   lat = "latitude",
                                   old_crs = "EPSG:4326",
                                   new_crs = "ESRI:54009") {
  mark_recapture_project$destination$recovery_data <- lapply(
    mark_recapture_project$destination$recovery_data,
    function(x) project_df(x, lon = lon, lat = lat)
  )

  mark_recapture_project$destination$window <- project_window(
    mark_recapture_project$destination$window
  )

  mark_recapture_project
}