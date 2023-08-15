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

#' Parameter values along a segmentized profile line
#'
#' This function segmentizes a continous line (sf-object) into points
#' to calculates the distance between these points
#'
#' @param profile_line coordinates of the starting point of the line as
#' longitude/latitude. E.g., c(0,0).
#' @param df_max_length parameter of the function sf::st_segmentize.
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#'
#' @return sfc-object including the profile line as points and the distance
#'  on the profile line
#' @export
#' @examples profile_points(profile_line(c(0, 0), c(1, 1), crs = "ESRI:54009"))
#'
profile_points <- function(profile_line, df_max_length = 60000) {
  profile_line <- sf::st_segmentize(profile_line, dfMaxLength = df_max_length)
  profile_line <- sf::st_cast(profile_line, "POINT")

  profile_line <- profile_line %>%
    dplyr::mutate(dist = sf::st_distance(.data$geometry)[, 1])

  profile_line
}
