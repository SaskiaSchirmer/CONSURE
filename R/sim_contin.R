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

#' simulation function for mark-recovery setting
#'
#' This function allows you to simulate data of dead recoveries with specified
#' survival, migratory connectivity and recovery probability in 1D- or 2D-space
#' and time using rejection sampling.
#' @param mark_recapture_object object of class mark_recapture_object
#' (see mark_recapture_object())
#' @param res spatial resolution. Defaults to 100.
#' @return returns object of class mark_recapture_object with added simulated
#' recovery data
#'
#' @export
#' @examples {
#'   mro <- mark_recapture_object(
#'     xrange = c(0, 1),
#'     survival = function(w) {
#'       stmp <- function(w) 0.5 * w + .4
#'       stmp(w / 100222.8)
#'     },
#'     recovery = function(w) 0.1,
#'     migratory_connectivity = function(b, w, B = 3) {
#'       mtmp <- function(b, w, B = B) {
#'         truncnorm::dtruncnorm(w, 0, 1,
#'           mean = seq(0.1, 0.9,
#'             length.out = B
#'           )[b],
#'           sd = 0.3
#'         )
#'       }
#'       mtmp(b, w / 100222.8, B)
#'     },
#'     marked_individuals = rep(1000, 3),
#'     observation_time = 3, robust = TRUE
#'   )
#'   mro <- sim_contin(mro)
#' }
#'
sim_contin <- function(mark_recapture_object, res = 100) {
  eta <- list()
  k <- numeric()
  o_t <- mark_recapture_object$observation_time
  origin_names <- names(mark_recapture_object$origins)[
    names(mark_recapture_object$origins) != "all"
  ]
  dim <- mark_recapture_object$spatial_dimension
  win <- mark_recapture_object$destination$window
  crs <- mark_recapture_object$destination$crs

  if (dim == 1) {
    normalize <- mark_recapture_object$destination$window$xrange[2] -
      mark_recapture_object$destination$window$xrange[1]
  } else if (dim == 2) {
    normalize <- spatstat.geom::area(win)
  }


  for (b in origin_names) {
    # calculate probability to be not seen independent of space and time for
    # every area of origin
    p <- (normalize - p_nf(b, mark_recapture_object)) / normalize

    # 1st step: simulate count of found individuals
    k[b] <- stats::rbinom(
      1, mark_recapture_object$origins[[b]]$marked_individuals,
      p
    )

    # 2nd step: sample data from subdensity
    # using rejection sampling

    if (rlang::is_installed("truncdist")) {
      if (mark_recapture_object$spatial_dimension != 1) {
        f_f2 <- function(x) {
          point <- sf::st_sfc(sf::st_point(x[2:1]),
            crs = "EPSG:4326"
          )
          point <- sf::st_transform(point, crs = crs)
          x[1:2] <- sf::st_coordinates(point)[2:1]
          f_f(w = c(x[1], x[2]), t = x[3], b = b, mark_recapture_object, p)
        }

        dg <- function(x) {
          prod(c(
            stats::dbeta(x[1], shape1 = 1, shape2 = 2),
            stats::dbeta(x[2], shape1 = 1, shape2 = 2),
            truncdist::dtrunc(x[3], "geom", 0, o_t, prob = 0.2)
          )) +
            0.0000000001
        }
        rg <- function(n) {
          c(
            stats::rbeta(n, shape1 = 1, shape2 = 2),
            stats::rbeta(n, shape1 = 1, shape2 = 2),
            truncdist::rtrunc(n, "geom", 0, o_t, prob = 0.2)
          )
        }
        cnames <- c("mark_area", "longitude", "latitude", "age")
      } else {
        f_f2 <- function(x) {
          x[1] <- x[1] * normalize
          f_f(w = x[1], t = x[2], b = b, mark_recapture_object, p)
        }

        dg <- function(x) {
          prod(c(
            stats::dbeta(x[1], shape1 = 1, shape2 = 2),
            truncdist::dtrunc(x[2], "geom", 0, o_t, prob = 0.2)
          )) +
            0.0000000001
        }
        rg <- function(n) {
          c(
            stats::rbeta(n, shape1 = 1, shape2 = 2),
            truncdist::rtrunc(n, "geom", 0, o_t, prob = 0.2)
          )
        }

        cnames <- c("mark_area", "longitude", "age")
      }
    } else {
      rlang::check_installed("truncdist")
    }

    if (rlang::is_installed("SimDesign")) {
      eta[[b]] <- SimDesign::rejectionSampling(k[b] + 1,
        df = f_f2, dg = dg,
        rg = rg, M = 10
      )
    } else {
      rlang::check_installed("SimDesign")
    }

    eta[[b]] <- as.data.frame(eta[[b]][-nrow(eta[[b]]), ])
    eta[[b]] <- cbind(b, eta[[b]], stringsAsFactors = FALSE)
    colnames(eta[[b]]) <- cnames

    if (!("latitude" %in% colnames(eta[[b]]))) {
      eta[[b]]$latitude <- 0
    }

    mark_recapture_object$origins[[b]]$number_of_recoveries <- unname(k[b])
  }

  mark_recapture_object$destination$recovery_data <-
    lapply(eta, function(x) {
      x$longitude <- x$longitude * (
        mark_recapture_object$destination$window$xrange[2] -
          mark_recapture_object$destination$window$xrange[1])
      x$latitude <- x$latitude * (
        mark_recapture_object$destination$window$yrange[2] -
          mark_recapture_object$destination$window$yrange[1])
      sf::st_as_sf(x,
        coords = c("longitude", "latitude"),
        crs = crs
      )
    })

  mark_recapture_object$origins[["all"]]$number_of_recoveries <- sum(k)

  return(mark_recapture_object)
}
