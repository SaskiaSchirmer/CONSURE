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

#' recovery probability estimator
#'
#' This function estimates the recovery probability uniformly over the whole
#' recovery area.
#' @param mark_recapture_object object of class mark_recapture_object
#' (see mark_recapture_object())
#' @param b name of the area of origin to estimate recovery for. Defaults to
#'  "all".
#' @return mark_recpature_object with scalar of recovery probability
#' @export
#' @examples mro <- est_r(mro1D)
est_r <- function(mark_recapture_object, b = "all") {
  k <- mark_recapture_object$origins[[b]]$number_of_recoveries
  c <- mark_recapture_object$estimates$c[[b]]
  n <- mark_recapture_object$origins[[b]]$marked_individuals

  mark_recapture_object$estimates[["r"]] <- c * k / n
  mark_recapture_object
}
