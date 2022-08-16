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

#' projects data.frame to another coordinate system
#'
#' This function projects a data.frame to another coordinate system, which
#' can be specified.
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
#' @examples new_df <- project_df(mro2D$destination$recovery_data$b1,
#'   old_crs = "ESRI:54009", new_crs = "EPSG:4326"
#' )
#'
project_df <- function(df, lon = "longitude",
                       lat = "latitude", old_crs = "EPSG:4326",
                       new_crs = "ESRI:54009") {
  df_sf <- sf::st_as_sf(df,
    coords = c(lon, lat)
  )
  sf::st_crs(df_sf) <- old_crs
  df_sf <- df_sf %>% sf::st_transform(new_crs)
  df_sf
}
