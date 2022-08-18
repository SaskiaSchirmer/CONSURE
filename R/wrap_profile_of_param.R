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

#' Parameter values along a profile line
#'
#' This function creates a linear profile line between two points,
#' segmentizes it into points and calculates the distance between the segments.
#' It is a wrapper function around all necessary steps. To customize this
#' procedure, use the functions inside.
#'
#' @param coord_start coordinates of the starting point of the line as
#' longitude/latitude. E.g., c(0,0).
#' @param coord_end coordinates of the end point of the line as
#' longitude/latitude. E.g., c(0,0).
#' @param param name of the parameter
#' @param b name of origin. Defaults to "all".
#' @inheritParams est_s
#' @importFrom dplyr %>%
#'
#' @return values of the given parameter along a profile line
#' @export
#' @examples wrap_profile_of_param(c(0, 0), c(1, 1), mro2D, "s")
#'
wrap_profile_of_param <- function(coord_start, coord_end,
                                  mark_recapture_object, param, b = "all") {
  crs <- mark_recapture_object$destination$crs
  profile_line <- profile_line(coord_start, coord_end, crs)

  profile_points <- profile_points(profile_line)

  rastered_param <- raster_param(mark_recapture_object, param, b)

  profile_of_parameter(rastered_param, profile_points)
}
