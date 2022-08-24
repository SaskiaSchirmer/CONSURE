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

#' True discrete migratory connectivity from a
#' continuous migratory connectivity function.
#'
#' This function integrates the continuous migratory connectivity function
#' for disjoint intervals given as a knot sequence and adds the discrete
#' migratory connectivity values to the
#' mark_recapture_object$breedingAreas[[breedingAreaName]]$mDiscrete.
#' This function is for simulated data only.
#'
#' @param mark_recapture_object an object of the class 'mark_recapture_object'
#' @param knots_prop knot sequence, list with vectors for latitude and
#' longitude, each vector contains W+1 entries which specify the boundaries
#' between the discrete non-breeding areas
#'
#' @return mark_recapture_object
#'
#' @export
#' @examples{
#'     kp <-list(latitude = c(0,0.25, 0.5, 0.75,1),
#'         longitude = NULL)
#'     mro <- calcDiscreteM(mark_recapture_object = mro1D, knots_prop = kp)
#' }


calc_discrete_m <- function(mark_recapture_object, knots_prop) {
  dim <- mark_recapture_object$spatial_dimension
  res <- mark_recapture_object$spatial_resolution
  knots <- knots_prop
  origin_names <- names(mark_recapture_object$origins)

  if (dim == 1) {
    if (sum(sapply(knots, is.null)) == 1) {
      knots <- unname(unlist(knots))
    } else {
      message("not sure how to use 'knots'")
    }

    bounds <- data.frame(
      lower_bound = knots[1:(length(knots) - 1)],
      upper_bound = knots[2:length(knots)]
    )

    i <- 1
    for (b in origin_names) {
      mark_recapture_object$origins[[b]]$m_discrete <-
        apply(bounds, 1, function(x) {
          stats::integrate(
            mark_recapture_object$origins[[b]]$migratory_connectivity,
            x[1], x[2]
          )$value
        }) * res
      i <- i + 1
    }
  } else if (dim == 2) {
    bounds_longitude <- data.frame(
      lower_bound_longitude = knots$longitude[1:(length(knots$longitude) - 1)],
      upper_bound_longitude = knots$longitude[2:length(knots$longitude)]
    )

    bounds_longitude <- bounds_longitude[rep(seq_len(nrow(bounds_longitude)),
      each = length(knots$latitude) - 1
    ), ]

    bounds_latitude <- data.frame(
      lowerBoundLatitude = knots$latitude[1:(length(knots$latitude) - 1)],
      upperBoundLatitude = knots$latitude[2:length(knots$latitude)]
    )

    bounds_latitude <- bounds_latitude[rep(
      seq_len(nrow(bounds_latitude)),
      length(knots$longitude) - 1
    ), ]

    bounds <- data.frame(cbind(bounds_longitude, bounds_latitude))

    i <- 1

    for (b in origin_names) {
      mark_recapture_object$origins[[b]]$m_discrete <- apply(
        bounds, 1, function(x) {
          cubature::adaptIntegrate(
            mark_recapture_object$origins[[b]]$migratory_connectivity,
            c(x[1], x[3]), c(x[2], x[4])
          )$integral
        }
      ) * res^2
      i <- i + 1
    }
  } else {
    message("not known how to calculate discrete migratory connectivity for
            this number of dimension.")
  }

  return(mark_recapture_object)
}
