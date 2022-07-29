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

#' estimate raw density of recovered individuals
#'
#' This function estimates the raw density of recovered individuals in space
#' and time from given data using a kernel density estimate.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param res resolution in space
#' @param all boolean: if TRUE only one kernel density estimate will be
#' calculated summarizing all breeding areas. Defaults to FALSE.
#' @param xname name of x variable, e.g. longitude
#' @param yname name of y variable, e.g. latitude
#' @param timename name of time variable, e.g. age
#' @param bw numeric. Spatial bandwidth parameter. Defaults to NULL.
#' @param lam numeric. temporal bandwidth parameter. Defaults to 1.1.
#' @return markRecaptureObject with list of values created by
#' sparr::spattemp.density (see ?sparr::spattemp.density for details) and
#' spatial resolution.
#' @export
#' @examples mro <- estKDE(mro1D)
estKDE <- function(markRecaptureObject, res = 100, all = FALSE,
                   xname = "longitude", yname = "latitude", timename = "age",
                   bw = NULL, lam = 1.1) {
  eta <- markRecaptureObject$winteringArea$recoveryData
  oT <- markRecaptureObject$observationTime
  win <- markRecaptureObject$winteringArea$window
  if (identical(win$yrange, c(0, 0))) {#win$yrange <- c(0, 123642.46)

  win <- spatstat.geom::owin(markRecaptureObject$winteringArea$window$xrange,
                    c(0,1))}
  breedingAreaNames <- names(markRecaptureObject$breedingAreas)[
    !grepl("all", names((markRecaptureObject$breedingAreas)))
  ]
  markRecaptureObject$spatialResolution <- res
  dim <- markRecaptureObject$spatialDim

  # if (dim == 1) {
  #   markRecaptureObject$inside <- spatstat.geom::as.mask(win, dimyx = res)$m
  #
  #   normalize <- sum(markRecaptureObject$inside, na.rm = TRUE)
  # } else if (dim == 2) {
  #   markRecaptureObject$inside <- spatstat.geom::as.mask(win, dimyx = res)$m
  #
  #   normalize <- sum(markRecaptureObject$inside, na.rm = TRUE)
  # }

  if (dim == 1) {
    markRecaptureObject$inside <- spatstat.geom::as.mask(win, dimyx = res)$m

    normalize <- markRecaptureObject$winteringArea$window$xrange[2]-
      markRecaptureObject$winteringArea$window$xrange[1]
  } else if (dim == 2) {
    markRecaptureObject$inside <- spatstat.geom::as.mask(win, dimyx = res)$m

    normalize <- spatstat.geom::area(win)
  }

  if (all) {
    eta <- list(do.call("rbind", eta))
    #x <- eta[[1]][, xname]
    x <- unname(sf::st_coordinates(eta[[1]])[,1])

    # y <- try(eta[[1]][, yname], silent = TRUE)
    # if ("try-error" %in% class(y)) {
    #   y <- stats::runif(length(eta[[1]][, xname]), 0, 1)
    # }

    # y <- try(unname(sf::st_coordinates(eta[[1]])[,2]), silent = TRUE)
    # if ("try-error" %in% class(y)) {
    #   y <- stats::runif(length(unname(sf::st_coordinates(eta[[1]])[,2])), 0, 1)
    # }

    y <- unname(sf::st_coordinates(eta[[1]])[,2])
    # if (length(table(y))==1) {
    #   y <- y+stats::runif(length(unname(sf::st_coordinates(eta[[1]])[,2])), 0, 1)
    # }


    pp <- spatstat.geom::ppp(x, y, window = win, marks = eta[[1]][[timename]])
    #pp <- spatstat.geom::ppp(x, y, window = win, marks = eta[[1]][, timename])
    if (is.null(bw)) {
    h <- sparr::OS(pp)
    } else {
      h <- bw
    }

    markRecaptureObject$kde[["all"]] <-
      sparr::spattemp.density(pp,
        h = h[1],
        tt = pp$marks,
        lambda = lam,
        tlim = c(1, oT),
        sedge = "uniform",
        tedge = "uniform",
        sres = res
      )

    intAllT <- sum(sapply(
      markRecaptureObject$kde[["all"]]$z,
      function(x) sum(x, na.rm = TRUE) #/ normalize*res
    ))

    for (t in 1:oT) {
      markRecaptureObject$kde[["all"]]$z[[t]] <-
        markRecaptureObject$kde[["all"]]$z[[t]] / intAllT * res * res
    }
  } else {
    for (b in breedingAreaNames) {
      #x <- eta[[b]][, xname]
      x <- unname(sf::st_coordinates(eta[[b]])[,1])

      y <- unname(sf::st_coordinates(eta[[b]])[,2])

      # y <- try(unname(sf::st_coordinates(eta[[b]])[,2]), silent = TRUE)
      # if ("try-error" %in% class(y)) {
      #   y <- stats::runif(length(unname(sf::st_coordinates(eta[[b]])[,2])), 0, 1)
      # }


      pp <- spatstat.geom::ppp(x, y, window = win, marks = eta[[b]][[timename]])

      # y <- try(eta[[b]][, yname], silent = TRUE)
      # if ("try-error" %in% class(y)) {
      #   y <- stats::runif(length(eta[[b]][, 1]), 0, 1)
      # }
      #
      #
      #
      # pp <- spatstat.geom::ppp(x, y, window = win, marks = eta[[b]][, timename])
      if (is.null(bw)) {
        h <- sparr::OS(pp)
      } else {
        h <- bw
      }
      print(h)
      markRecaptureObject$kde[[b]] <-
        sparr::spattemp.density(pp,
          h = h[1],
          tt = pp$marks,
          lambda = lam,
          tlim = c(1, oT),
          sedge = "uniform",
          tedge = "uniform",
          sres = res
        )

      intAllT <- sum(sapply(
        markRecaptureObject$kde[[b]]$z,
        function(x) sum(x, na.rm = TRUE) #/ normalize*res
      ))

      for (t in 1:oT) {
        markRecaptureObject$kde[[b]]$z[[t]] <-
          markRecaptureObject$kde[[b]]$z[[t]] / intAllT*res*res
      }
    }
  }

  return(markRecaptureObject)
}
