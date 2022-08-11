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
#' This function calculates the profile of a raster object along a segmented
#' profile line.
#'
#' @param rastered_param raster stack of the parameter values and the
#' bootstrap confidence interval
#' @param profile_points sfc-object including the profile line as points and
#' the distance on the profile line
#' @param param name of the parameter
#'
#' @return values of the given parameter along a profile line
#' @export
#' @examples profile_of_parameter(
#'   rasterParam(mro2D, "s", "all"),
#'   profile_points(profile_line(c(0, 0), c(1, 1)))
#' )
#'
profile_of_parameter <- function(rastered_param, profile_points) {
  param <- terra::extract(rastered_param, terra::vect(profile_points))
  profile_of_parameter <- cbind(profile_points, param)

  names(profile_of_parameter)[names(profile_of_parameter) == "lyr.1"] <-
    "parameter_value"

  if ("lyr.1.1" %in% names(profile_of_parameter)) {
    names(profile_of_parameter)[names(profile_of_parameter) == "lyr.1.1"] <-
      "bootstrap_upper_quantile"
  }

  if ("lyr.1.2" %in% names(profile_of_parameter)) {
    names(profile_of_parameter)[names(profile_of_parameter) == "lyr.1.2"] <-
      "bootstrap_lower_quantile"
  }

  profile_of_parameter
}
