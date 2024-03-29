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

#' constructor for destination area
#'
#' This function defines the properties of the destination area.
#' @param window object of class "owin":observation window in
#'               two-dimensional plane
#' @param crs coordinate system of destination
#' @param survival function: survival function defined over the whole
#'                 destination area, independent of the origin
#' @param recovery constant function: recovery probability, must be
#'                 constant over whole destination area
#' @param recovery_data empty space for simulated or real-world recovery data
#' @return object of class "destination": contains list of window, survival
#'         and recovery for the destination area


new_destination <- function(window = spatstat.geom::owin(),
                            crs,
                            survival,
                            recovery,
                            recovery_data) {
  stopifnot(spatstat.geom::is.owin(window))
  stopifnot(is.null(survival) | is.function(survival))
  stopifnot(is.null(recovery) | is.function(recovery))
  structure(list(
    window = window, crs = crs, survival = survival, recovery = recovery,
    recovery_data = recovery_data
  ), class = "destination")
}

#' helper function for destination area
#'
#' This function defines the properties of the destination area using the
#' constructor.
#' @inheritParams new_destination
#' @param xrange vector in the form of c(xmin,xmax). To define line or rectangle
#' xrange and yrange can be used instead of window.
#' @param yrange vector in the form of c(ymin,ymax). To define line or rectangle
#' xrange and yrange can be used instead of window.
#' @return object of class "destination": contains list of window, survival
#' and recovery for the destination area
#' @export
#' @examples{
#'      w_a <- destination(survival = function(w) 0.3,
#'      recovery = function(w) 0.01,
#'      xrange = c(0,1),
#'      crs = "EPSG:4326" )
#'  }

destination <- function(window = NULL, survival, recovery, xrange = c(0, 0),
                        yrange = c(0, 0), crs,
                        recovery_data = NULL) {
  if (is.null(window) &&
    identical(xrange, c(0, 0)) &&
    identical(yrange, c(0, 0))) {
    stop("Please define either window or x- and/or y-range of the destination
         area")
  } else {
    if (!spatstat.geom::is.owin(window)) {
      window <- spatstat.geom::as.owin(list(xrange = xrange, yrange = yrange))
    }


    if (crs != "ESRI:54009") {
      window <- project_window(window, old_crs = crs)
      if (!is.null(recovery_data)) {
        recovery_data <- lapply(recovery_data, function(x) project_df(x))
      }

      crs <- "ESRI:54009"
    }
    return(new_destination(
      window, crs,
      survival, recovery, recovery_data
    ))
  }
}

#' constructor for the origin
#'
#' This function defines the properties of the origin.
#' @param marked_individuals integer: number of marked individuals in this
#' origin
#' @param number_of_recoveries numeric vector. Number of recoveries belonging to
#' each origin
#' @param migratory_connectivity function: migratory connectivity function
#' conditioned on this origin defined over the whole destination area
#' @return object of class "origin": contains list of number of marked
#' individuals and migratory connectivity function


new_origin <- function(marked_individuals = numeric(),
                       number_of_recoveries,
                       migratory_connectivity) {
  stopifnot(is.null(migratory_connectivity) |
    is.function(migratory_connectivity))
  structure(list(
    marked_individuals = marked_individuals,
    number_of_recoveries = number_of_recoveries,
    migratory_connectivity = migratory_connectivity
  ),
  class = "origin"
  )
}

#' helper function for origin
#'
#' This function defines the properties of the origin using the
#' constructor.
#' @inheritParams new_origin
#' @return object of class "origin": contains list of number of marked
#' individuals and migratory connectivity function
#' @export
#' @examples{
#'  migratory_connectivity = function(b,w,B=B) {
#'    truncnorm::dtruncnorm(w,0,1, mean = seq(0.1,0.9,length.out = 5)[b],
#'    sd = 0.3)
#'  }
#' mig_con <- functional::Curry(migratory_connectivity,b=1,B=5)
#' b_a <- origin(marked_individuals = 10000,
#'     number_of_recoveries = NULL,
#'     migratory_connectivity = mig_con)
#' }

origin <- function(marked_individuals, number_of_recoveries,
                   migratory_connectivity) {
  new_origin(marked_individuals, number_of_recoveries, migratory_connectivity)
}

#' constructor for mark recapture object
#'
#' This function defines the properties of the mark recapture object.
#' @param destination object of class "destination"
#' @param origins list of objects of class "origins"
#' @param observation_time single integer. length of observation window in years
#' @param number_of_origins single integer. number of origins.
#' @param spatial_dimension single integer. spatial dimensions, should only be
#' 1 or 2.
#' @param robust logical if TRUE robust linear model is calculated to estimate
#' survival and migratory connectivity
#' @return object of class "mark_recapture_object": contains a list of
#' destinations, origins, observation time, number of origins, spatial
#' dimensions, empty slots for the spatial resolution, the kernel density
#' estimate and the estimates, the class of this object is
#' "mark_recapture_object"

new_mark_recapture_object <- function(destination, origins,
                                      observation_time, number_of_origins,
                                      spatial_dimension, robust) {
  stopifnot(class(destination) == "destination")
  stopifnot(is.list(origins))
  stopifnot(observation_time %% 1 == 0)
  stopifnot(length(number_of_origins) == 1)
  stopifnot(number_of_origins %% 1 == 0)
  stopifnot(is.logical(robust))

  structure(list(
    destination = destination,
    origins = origins,
    observation_time = observation_time,
    number_of_origins = number_of_origins,
    spatial_dimension = spatial_dimension,
    spatial_resolution = NULL,
    robust = robust,
    kde = list(),
    estimates = list()
  ), class = "mark_recapture_object")
}

#' helper function for mark recapture object
#'
#' This function defines the properties of the mark recapture object using the
#' constructor.
#' @inheritParams destination
#' @param marked_individuals integer: number of marked individuals in this
#' origin
#' @param migratory_connectivity either a list of functions containing one
#' migratory connectivity function for every origin or a function with
#' parameter b, allowing to partialize the function for every origin with
#' purrr::partial
#' @param observation_time length of observation window in years
#' @param robust logical if TRUE robust linear model is calculated to estimate
#' survival and migratory connectivity
#' @param real_recoveries real-world recovery data, defaults to NULL
#' @param origin_names character vector with origin names, defaults
#' to NULL
#' @param crs coordinate system. Defaults to "EPSG:4326" (longitude/latitude).
#' @return object of class "mark_recapture_object": contains a list of the
#' destination, origins, observation time, number of origins and
#' spatial dimension
#' @export
#' @examples{
#' mro <- mark_recapture_object(xrange = c(0,1),
#'     survival = function(w) {0.5*w+.4},
#'     recovery = function(w) {0.01},
#'     marked_individuals = rep(100000,5) ,
#'     migratory_connectivity = function(b,w,B=B) {
#'         truncnorm::dtruncnorm(w,0,1, mean = seq(0.1,0.9,length.out = B)[b],
#'         sd = 0.3)
#'         },
#'     observation_time = 10)
#' }

mark_recapture_object <- function(window = NULL,
                                  xrange = c(0, 0),
                                  yrange = c(0, 0),
                                  survival = NULL,
                                  recovery = NULL,
                                  marked_individuals,
                                  migratory_connectivity = NULL,
                                  observation_time,
                                  real_recoveries = NULL,
                                  origin_names = NULL,
                                  robust = TRUE,
                                  crs = "EPSG:4326") {
  number_of_origins <- length(marked_individuals)

  spatial_dimension <- 2
  if (identical(window$yrange, c(0, 0)) ||
    (is.null(window) &&
      identical(yrange, c(0, 0)))) {
    spatial_dimension <- 1
  }


  if (is.data.frame(real_recoveries)) {
    if (sum(colnames(real_recoveries) %in%
      c("mark_area", "longitude", "latitude", "age") |
      colnames(real_recoveries) %in%
        c("mark_area", "age", "geometry")) !=
      length(colnames(real_recoveries))) {
      message("Your recovery data does not have the default column names.
              You can either use CONSURE::renameData() or you must specify
              the colnames in the functions.")
    }

    if (!is.factor(real_recoveries$mark_area)) {
      real_recoveries$mark_area <- factor(real_recoveries$mark_area)
    }

    tmp <- list()
    for (area in levels(real_recoveries$mark_area)) {
      tmp[[area]] <- real_recoveries[real_recoveries$mark_area == area, ]
    }
  } else {
    tmp <- real_recoveries
  }

  destination <- destination(window,
    crs = crs,
    survival, recovery,
    xrange, yrange,
    recovery_data = tmp
  )
  origins <- list()

  if (!is.null(real_recoveries)) {
    number_of_recoveries <- rec_inds_func(origin_names, real_recoveries)
  }

  if (is.null(origin_names)) {
    origin_names <- paste("b", 1:number_of_origins, sep = "")
  }

  if (!is.null(migratory_connectivity)) {
    if (is.list(migratory_connectivity)) {
      for (b in 1:number_of_origins) {
        origins[[origin_names[b]]] <-
          origin(
            marked_individuals = marked_individuals[b],
            number_of_recoveries = NULL,
            migratory_connectivity = migratory_connectivity[[b]]
          )
      }
      origins[["all"]] <-
        origin(
          marked_individuals = sum(marked_individuals[b]),
          number_of_recoveries = NULL,
          migratory_connectivity = function(w) {
            tmp <- matrix(NA, ncol = length(w), nrow = number_of_origins)
            for (b in 1:number_of_origins) {
              tmp[b, ] <- marked_individuals[b] * migratory_connectivity[[b]](w)
            }
            colSums(tmp) / sum(marked_individuals)
          }
        )
    } else {
      tmp_mig <- list()
      for (b in 1:number_of_origins) {
        if (rlang::is_installed("functional")) {
          tmp_mig[[b]] <- functional::Curry(migratory_connectivity,
            b = b,
            B = number_of_origins
          )
        } else {
          rlang::check_installed("functional")
        }

        origins[[origin_names[b]]] <-
          origin(
            marked_individuals = marked_individuals[b],
            number_of_recoveries = NULL,
            migratory_connectivity = tmp_mig[[b]]
          )
      }

      if (spatial_dimension == 2) {
        origins[["all"]] <-
          origin(
            marked_individuals = sum(marked_individuals),
            number_of_recoveries = NULL,
            migratory_connectivity = function(w) {
              tmp <- numeric()
              for (b in 1:number_of_origins) {
                tmp[b] <- marked_individuals[b] * tmp_mig[[b]](w)
              }
              sum(tmp) / sum(marked_individuals)
            }
          )
      } else if (spatial_dimension == 1) {
        origins[["all"]] <-
          origin(
            marked_individuals = sum(marked_individuals),
            number_of_recoveries = NULL,
            migratory_connectivity = Vectorize(
              function(w) {
                tmp <- numeric()
                for (b in 1:number_of_origins) {
                  tmp[b] <- marked_individuals[b] * tmp_mig[[b]](w)
                }
                sum(tmp) / sum(marked_individuals)
              }
            )
          )
      }
    }
  } else {
    for (b in 1:number_of_origins) {
      origins[[origin_names[b]]] <-
        origin(
          marked_individuals = marked_individuals[b],
          number_of_recoveries = number_of_recoveries[b],
          migratory_connectivity = migratory_connectivity
        )
    }

    origins[["all"]] <-
      origin(
        marked_individuals = sum(marked_individuals),
        number_of_recoveries = sum(number_of_recoveries),
        migratory_connectivity = migratory_connectivity
      )
  }



  new_mark_recapture_object(
    destination = destination,
    origins = origins,
    observation_time = observation_time,
    number_of_origins = number_of_origins,
    spatial_dimension = spatial_dimension,
    robust = robust
  )
}
