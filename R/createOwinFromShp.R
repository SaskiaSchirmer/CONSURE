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

#' creates an owin-object from a shpfile
#'
#' This function creates an owin-object from a shpfile adding discrete
#' non-breeding areas if applicable.
#' @param file character, containing the path to the shape file. The shape file
#' includes at least the boundaries of the continuous non-breeding area. It may
#' contain fields not relevant for the analysis. For the combined approach one
#' field must contain information on to which discrete non-breeding area each
#' geometry belongs (see nonbreedingField).
#' @param crs coordinate reference system. Defaults to
#' "+proj=utm +zone=31N +datum=WGS84". For details see ?sp::CRS.
#' @param nonbreedingField only used for the combined model approach. Name of
#' the field containing the information on the discrete non-breeding area in
#' the shape file. Defaults to NULL.
#' @return owin object only containing the geometry and, in case it is used for
#' the combined approach, information on the discrete non-breeding area each
#' geometry belongs to
#' @export
#' @examples \dontrun{createOwinFromShp(file)}

createOwinFromShp <- function(file, crs = "+proj=utm +zone=31N +datum=WGS84",
                              nonbreedingField = NULL){
  shpfile <- sf::st_read(file)
  shpfile <- sf::st_transform(shpfile, sp::CRS(crs))

  if(!is.null(nonbreedingName)){
    shpfile <- shpfile[,nonbreedingField]
  } else {
    shpfile <- sf::st_geometry(shpfile)
  }

  return(spatstat.geom::as.owin(shpfile))
}
