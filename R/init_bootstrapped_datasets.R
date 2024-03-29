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

#' This function initializes a list of bootstrapped data sets for which in a
#' next step the parameters may be estimated.
#' @param iterations number of bootstraps
#' @inheritParams est_s
#' @param filename (path to file) and filename to store list of bootstrapped
#'  data.
#'
#' @return  list of bootstrapped datasets
#' @export
#' @examples bootstrap_mro <- init_bootstrapped_datasets(
#'   mark_recapture_object = mro1D,
#'   filename = "mro1D_bootstrap_raw"
#' )
#'
init_bootstrapped_datasets <- function(iterations = 1000,
                                       mark_recapture_object, filename) {
  ls <- vector(mode = "list", length = iterations)

  bootstrap_data <- lapply(ls, FUN = function(x) {
    tmp <- bootstrap_marking_data(mark_recapture_object)
    list(data = tmp)
  })
  save(iterations, mark_recapture_object, bootstrap_data,
    file = paste(filename, ".Rdata", sep = "")
  )

  bootstrap_data
}
