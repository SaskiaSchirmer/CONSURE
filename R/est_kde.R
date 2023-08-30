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

#' estimate raw density of recovered individuals
#'
#' This function estimates the raw density of recovered individuals in space
#' and time from given data using a kernel density estimate.
#' @param mark_recapture_object object of class mark_recapture_object
#' (see mark_recapture_object())
#' @param res resolution in space
#' @param all boolean: if TRUE only one kernel density estimate will be
#' calculated summarizing all areas of origin. Defaults to FALSE.
#' @param xname name of x variable, e.g. longitude
#' @param yname name of y variable, e.g. latitude
#' @param timename name of time variable, e.g. age
#' @param bw numeric. Spatial bandwidth parameter. Defaults to NULL.
#' @param lam numeric. temporal bandwidth parameter. Defaults to 1.1.
#' @return mark_recapture_object with list of values created by
#' sparr::spattemp.density (see ?sparr::spattemp.density for details) and
#' spatial resolution.
#' @export
#' @examples mro <- est_kde(mro1D)
est_kde <- function(mark_recapture_object, res = 100, all = FALSE,
                    xname = "longitude", yname = "latitude", timename = "age",
                    bw = NULL, lam = 1.1) {
  eta <- mark_recapture_object$destination$recovery_data
  o_t <- mark_recapture_object$observation_time
  win <- mark_recapture_object$destination$window
  if (identical(win$yrange, c(0, 0))) {
    win <- spatstat.geom::owin(
      win$xrange,
      c(0, 1)
    )
  }
  origin_names <- names(mark_recapture_object$origins)[
    !grepl("all", names((mark_recapture_object$origins)))
  ]
  mark_recapture_object$spatial_resolution <- res
  dim <- mark_recapture_object$spatial_dimension

  if (dim == 1) {
    mark_recapture_object$inside <- spatstat.geom::as.mask(win, dimyx = res)$m
  } else if (dim == 2) {
    mark_recapture_object$inside <- spatstat.geom::as.mask(win, dimyx = res)$m
  }

  if (all) {
    eta <- list(do.call("rbind", eta))
    x <- unname(sf::st_coordinates(eta[[1]])[, 1])

    y <- unname(sf::st_coordinates(eta[[1]])[, 2])

    pp <- spatstat.geom::ppp(x, y, window = win, marks = eta[[1]][[timename]])

    if (rlang::is_installed("sparr")) {
      if (is.null(bw)) {
        h <- sparr::OS(pp)
      } else {
        h <- bw
      }

      mark_recapture_object$kde[["all"]] <-
        sparr::spattemp.density(pp,
          h = h[1],
          tt = pp$marks,
          lambda = lam,
          tlim = c(1, o_t),
          sedge = "uniform",
          tedge = "uniform",
          sres = res
        )
    } else {
      rlang::check_installed("sparr")
    }


    integral_over_observation_time <- sum(sapply(
      mark_recapture_object$kde[["all"]]$z,
      function(x) sum(x, na.rm = TRUE)
    ))

    for (t in 1:o_t) {
      mark_recapture_object$kde[["all"]]$z[[t]] <-
        mark_recapture_object$kde[["all"]]$z[[t]] /
          integral_over_observation_time * res * res
    }
  } else {
    for (b in origin_names) {
      x <- unname(sf::st_coordinates(eta[[b]])[, 1])

      y <- unname(sf::st_coordinates(eta[[b]])[, 2])

      pp <- spatstat.geom::ppp(x, y, window = win, marks = eta[[b]][[timename]])

      if (is.null(bw)) {
        h <- sparr::OS(pp)
      } else {
        h <- bw
      }
      print(h)
      mark_recapture_object$kde[[b]] <-
        sparr::spattemp.density(pp,
          h = h[1],
          tt = pp$marks,
          lambda = lam,
          tlim = c(1, o_t),
          sedge = "uniform",
          tedge = "uniform",
          sres = res
        )

      integral_over_observation_time <- sum(sapply(
        mark_recapture_object$kde[[b]]$z,
        function(x) sum(x, na.rm = TRUE)
      ))

      for (t in 1:o_t) {
        mark_recapture_object$kde[[b]]$z[[t]] <-
          mark_recapture_object$kde[[b]]$z[[t]] /
            integral_over_observation_time * res * res
      }
    }
  }

  return(mark_recapture_object)
}
