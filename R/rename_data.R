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

#' rename recovery data with default column names
#'
#' This function renames the recovery data with default column names.
#' @param recovery_data data.frame with dead recoveries, containing information
#' on the area of origin, geographical coordinates of recovery and age at death
#' @param xname character, name of column containing information on longitude,
#' specify this in one-dimensional case
#' @param yname character, name of column containing information on latitude,
#' defaults to NULL, do not specify in one-dimensional case, but use xname
#' instead
#' @param timename character, name of column containing information on age at
#' death
#' @param mark_area_name character, name of column containing information on
#' the area of origin
#' @return recovery_data with default column names
#' @export
#' @examples{
#'     rename_data(recovery_data = robins_init$recovery_data,
#'                xname        = "recLon",
#'                yname        = "recLat",
#'                timename     = "age",
#'                mark_area_name = "mark_area")
#' }
rename_data <- function(recovery_data, xname, yname = NULL, timename,
                        mark_area_name) {
  if (is.null(yname)) {
    colnames_matrix <- matrix(
      c(
        mark_area_name, xname, timename,
        "mark_area", "longitude", "age"
      ),
      ncol = 2, nrow = 3
    )
  } else {
    colnames_matrix <- matrix(
      c(
        mark_area_name, xname, yname, timename,
        "mark_area", "longitude", "latitude", "age"
      ),
      ncol = 2, nrow = 4
    )
  }

  colnames(colnames_matrix) <- c("real", "default")

  index <- match(colnames_matrix[, "real"], colnames(recovery_data))

  colnames(recovery_data) <- colnames_matrix[, "default"][index]
  return(recovery_data)
}
