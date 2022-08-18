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


#' This function bootstraps the marking data for every area of origin.
#' @inheritParams est_s
#' @param crs coordinate system. Defaults to NULL.
#' @return  mark_recapture_object with estimates of all selected parameters
#' @export
#' @examples mro <- bootstrap_marking_data(mro1D)
#'
bootstrap_marking_data <- function(mark_recapture_object, crs = NULL) {
  destination <- mark_recapture_object$destination
  origin_names <- names(destination$recovery_data)
  ls <- as.list(origin_names[origin_names != "all"])
  spatial_resolution <- mark_recapture_object$spatial_resolution

  if (is.null(crs)) {
    crs <- mark_recapture_object$destination$crs
  }


  out <- lapply(ls, FUN = function(x) {
    origin <- mark_recapture_object$origins[[x]]
    recovery_data <- destination$recovery_data[[x]]
    marked_individuals <- origin$marked_individuals
    number_of_recoveries <- origin$number_of_recoveries

    marking_index <- sample(1:marked_individuals, replace = TRUE)
    recovery_index <- marking_index[marking_index <= number_of_recoveries]

    list(
      mark_area = x,
      real_recoveries = recovery_data[recovery_index, ],
      marked_individuals = length(
        marking_index[marking_index > number_of_recoveries]
      ) +
        length(recovery_index)
    )
  })


  real_recoveries <- do.call(
    "rbind",
    lapply(out, function(x) x$real_recoveries)
  )
  marked_individuals <- sapply(out, function(x) x$marked_individuals)

  bootstrapped_mark_recapture <- mark_recapture_object(
    window = mark_recapture_object$destination$window,
    real_recoveries = real_recoveries,
    marked_individuals = marked_individuals,
    observation_time = mark_recapture_object$observation_time,
    origin_names = origin_names,
    crs = crs
  )

  bootstrapped_mark_recapture$spatial_resolution <- spatial_resolution


  return(bootstrapped_mark_recapture)
}
