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

#' calculates number of recovered individuals from marked and recoveries data
#' frame
#'
#' This function calculates the number of recovered individuals marked in one
#' breeding area given the data frame of dead recoveries and a vector of names
#' of breeding areas
#' @param breedingAreaNames character vector with names of breeding areas
#' @param recoveries data frame of recoveries with column on marking location
#' @return vector of number of recovered individuals per breeding area
#' @export
#' @examples recIndsFunc(c("early", "late"), robinsInit$recoveryData)
recIndsFunc <- function(breedingAreaNames, recoveries) {
  as.numeric(table(recoveries$markArea)[
    match(breedingAreaNames, names(table(recoveries$markArea)))
  ])
}
