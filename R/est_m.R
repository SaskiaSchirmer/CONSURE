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

#' estimate migratory connectivity
#'
#' This function estimates the migratory connectivity in space constant over
#' time when survival and raw distribution of dead recoveries for every
#' area of origin is known.
#' @param mark_recapture_object object of class mark_recapture_object
#' (see mark_recapture_object())
#' @param all boolean: if TRUE only one kernel density estimate will be
#' calculated summarizing all areas of origin. Defaults to FALSE.
#' @return list of vectors with length res-1 containing migratory connectivity
#' density of every spot
#' @export
#' @examples mro <- est_m(mro1D, all = TRUE)
est_m <- function(mark_recapture_object, all = FALSE) {
  s_fit <- mark_recapture_object$estimates$s
  origin_names <- names(mark_recapture_object$origins)[
    !grepl("all", names((mark_recapture_object$origins)))
  ]
  dim <- mark_recapture_object$spatial_dimension

  if (all) {
    lm <- mark_recapture_object$estimates$lm$all
    mark_recapture_object$estimates[["m"]][["all"]] <-
      exp(lm$intercept - log(1 - s_fit))
    if (dim == 1) {
      mark_recapture_object$estimates[["c"]]["all"] <-
        sum(mark_recapture_object$estimates$m$all) /
          sum(colSums(mark_recapture_object$inside) > 0)
      mark_recapture_object$estimates[["m"]][["all"]] <-
        mark_recapture_object$estimates[["m"]][["all"]] /
          mark_recapture_object$estimates[["c"]]["all"] *
          (colSums(mark_recapture_object$inside) > 0)
    } else if (dim == 2) {
      mark_recapture_object$estimates[["c"]]["all"] <-
        sum(mark_recapture_object$estimates[["m"]][["all"]], na.rm = TRUE) /
          sum(mark_recapture_object$inside)
      mark_recapture_object$estimates[["m"]][["all"]] <-
        mark_recapture_object$estimates[["m"]][["all"]] /
          mark_recapture_object$estimates[["c"]]["all"] *
          mark_recapture_object$inside
    }
  } else {
    for (b in origin_names) {
      mark_recapture_object <- est_lm(mark_recapture_object,
        b = b,
        fixed_slope = mark_recapture_object$estimates$s
      )
      lm <- mark_recapture_object$estimates$lm[[b]]
      mark_recapture_object$estimates[["m"]][[b]] <-
        exp(lm$intercept - log(1 - s_fit))
      if (dim == 1) {
        mark_recapture_object$estimates[["c"]][b] <-
          sum(mark_recapture_object$estimates[["m"]][[b]]) /
            sum(colSums(mark_recapture_object$inside) > 0)
        mark_recapture_object$estimates[["m"]][[b]] <-
          mark_recapture_object$estimates[["m"]][[b]] /
            mark_recapture_object$estimates[["c"]][b] *
            (colSums(mark_recapture_object$inside) > 0)
      } else if (dim == 2) {
        mark_recapture_object$estimates[["c"]][b] <-
          sum(mark_recapture_object$estimates[["m"]][[b]], na.rm = TRUE) /
            sum(mark_recapture_object$inside)
        mark_recapture_object$estimates[["m"]][[b]] <-
          mark_recapture_object$estimates[["m"]][[b]] /
            mark_recapture_object$estimates[["c"]][b] *
            mark_recapture_object$inside
      }
    }
  }

  return(mark_recapture_object)
}
