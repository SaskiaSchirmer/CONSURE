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

#' linear model of kernel density estimate
#'
#' This function estimates the uncertainty of the parameters survival, migratory
#' connectivity and recovery probability by bootstrapping the marking data.
#' @inheritParams est_s
#' @param parameters vector of characters specifying the parameter names
#' considered for uncertainty estimation, possible values are s for survival,
#' m for migratory connectivity and r for recovery probability. Defaults to
#' c("s","m","r").
#' @return  mark_recapture_object with estimates of all selected parameters
#' @export
#' @examples mro <- est_parameters(mro1D)
#'
est_parameters <- function(mark_recapture_object, res = NULL,
                           parameters = c("s", "m", "r")) {
  if (is.null(res)) {
    if (!is.null(mark_recapture_object$spatial_resolution)) {
      res <- mark_recapture_object$spatial_resolution
    } else {
      message("Please provide a spatial resolution in `res`.")
    }
  }

  mark_recapture_object <- est_kde(mark_recapture_object, res, all = TRUE)
  mark_recapture_object <- est_kde(mark_recapture_object, res)

  mark_recapture_object <- est_s(mark_recapture_object = mark_recapture_object)

  mark_recapture_object <- est_m(mark_recapture_object = mark_recapture_object)
  mark_recapture_object <- est_m(
    mark_recapture_object = mark_recapture_object,
    all = TRUE
  )

  mark_recapture_object <- est_r(mark_recapture_object)

  return(mark_recapture_object)
}
