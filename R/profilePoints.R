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

#' Parameter values along a segmentized profile line
#'
#' This function segmentizes a continous line (sf-object) into points
#' to calculates the distance between these points
#'
#' @param profileLine coordinates of the starting point of the line as
#' longitude/latitude. E.g., c(0,0).
#' @param dfMaxLength parameter of the function sf::st_segmentize.
#'
#' @importFrom dplyr %>%
#'
#' @return sfc-object including the profile line as points and the distance
#'  on the profile line
#' @export
#' @examples profilePoints(profileLine(c(0,0),c(1,1)))

  profilePoints <- function(profileLine, dfMaxLength = 60000){

    profileLine$id = 1:nrow(profileLine)
    profileLine = sf::st_segmentize(profileLine, dfMaxLength = dfMaxLength)
    profileLine = sf::st_cast(profileLine, "POINT")

    profileLine = profileLine %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(dist = sf::st_distance(geometry)[, 1])

    profileLine
  }
