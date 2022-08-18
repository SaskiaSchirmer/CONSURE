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

#' Creates a linear profile line.
#'
#' This function converts the start and end points from longitude/latitude
#' to utm coordinates and creates a continuous line between these points as a
#' simple feature object.
#'
#' @param coord_start coordinates of the starting point of the line as
#' longitude/latitude. E.g., c(0,0).
#' @param coord_end coordinates of the end point of the line as
#' longitude/latitude. E.g., c(0,0).
#' @param crs coordinate system of mark_recapture_object
#' @importFrom dplyr %>%
#'
#' @return profile line as simple feature object.
#' @export
#' @examples profile_line(c(0, 0), c(1, 1), crs = "ESRI:54009")
#'
profile_line <- function(coord_start, coord_end, crs) {
  profile_line <- rbind(coord_start, coord_end) %>%
    sf::st_multipoint() %>%
    sf::st_linestring() %>%
    sf::st_sfc(crs = "EPSG:4326") %>%
    sf::st_transform(crs = crs) %>%
    sf::st_sfc() %>%
    sf::st_sf()

  profile_line
}
