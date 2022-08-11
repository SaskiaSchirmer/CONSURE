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

#' calculates number of recovered individuals from marked and recoveries data
#' frame
#'
#' This function calculates the number of recovered individuals marked in one
#' area of origin given the data frame of dead recoveries and a vector of names
#' of areas of origin
#' @param origin_names character vector with names of origins
#' @param recoveries data frame of recoveries with column on marking location
#' @return vector of number of recovered individuals per area of origin
#' @export
#' @examples rec_inds_func(c("early", "late"), robinsInit$recovery_data)
rec_inds_func <- function(origin_names, recoveries) {
  as.numeric(table(recoveries$mark_area)[
    match(origin_names, names(table(recoveries$mark_area)))
  ])
}
