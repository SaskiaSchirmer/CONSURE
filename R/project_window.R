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

#' projects owin-object to another coordinate system
#'
#' This function projects an owin object to another coordinate system, which
#' can be specified.
#' @param win owin-object
#' @param old_crs coordinate system of the input owin-object. Defaults to
#' longitude/latitude "EPSG:4326"
#' @param new_crs coordinate system of the output owin-object. Defaults to the
#' Mollweide Projection "ESRI:54009"
#' @return owin-object with projected coordinate-system
#' @export
#' @examples new_win <- project_window(mro2D$winteringArea$win)
#'
project_window <- function(win, old_crs = "EPSG:4326",
                           new_crs = "ESRI:54009") {
  win_sf <- sf::st_as_sf(win)
  sf::st_crs(win_sf) <- old_crs
  win_sf <- win_sf %>% sf::st_transform(new_crs)
  win_owin <- spatstat.geom::as.owin(win_sf)
  win_owin
}
