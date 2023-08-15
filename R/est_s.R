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

#' survival estimator
#'
#' This function estimates the survival from a kernel density estimate of the
#' data of recovered individuals. It uses the data of all areas of origin at
#' once.
#' @param mark_recapture_object object of class mark_recapture_object
#' (see mark_recapture_object())
#' @return mark_recapture_object vector/matrix of length res with survival
#' probabilities dependent on space
#' @export
#' @examples mro <- est_s(mro1D)
est_s <- function(mark_recapture_object) {
  mark_recapture_object <- est_lm(mark_recapture_object,
    b = "all"
  )
  mark_recapture_object$estimates[["s"]] <-
    exp(mark_recapture_object$estimates[["lm"]][["all"]][["slope"]])

  return(mark_recapture_object)
}
