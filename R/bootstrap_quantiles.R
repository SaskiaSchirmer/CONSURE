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

#' 0.25- and 0.95- confidence intervals for bootstrapped data.
#'
#' This function estimates the uncertainty of the parameters survival, migratory
#' connectivity and recovery probability by bootstrapping the marking data.
#' @inheritParams est_s
#' @param params vector of characters specifying the parameter names
#' considered for uncertainty estimation, possible values are s for survival,
#' m for migratory connectivity and r for recovery probability. Defaults to
#' c("s","m","r").
#' @param filename (path to file) and filename to store the
#' mark_recapture_object with bootstrap estimates.
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#'
#' @return  raw bootstrap dataframe to be used in bootstrap_quantiles
#' @export
#' @examples{\dontrun{
#'   mro  <- mro1D
#'   mro <- est_uncertainty(mro,parameters = "s", iterations = 2,
#'   filename = "test")
#'   mro <- bootstrap_quantiles(mro, "s", "test")}
#' }
#'
bootstrap_quantiles <- function(mark_recapture_object,
                                params = c("s", "m", "r"),
                                filename) {
  raw_bootstrap <- do.call("rbind", lapply(
    params,
    function(x) {
      get_bootstrap_parameters(
        mark_recapture_object, x
      )
    }
  ))

  mark_recapture_object$estimates$bootstrap$raw_bootstrap <- raw_bootstrap

  mark_recapture_object$estimates$bootstrap$bootstrap_quantiles <-
    mark_recapture_object$estimates$bootstrap$raw_bootstrap %>%
    dplyr::group_by(
      .data$latitude, .data$longitude,
      .data$mark_area, .data$parameter
    ) %>%
    dplyr::summarise(
      uq = stats::quantile(.data$value, 0.975, na.rm = TRUE),
      lq = stats::quantile(.data$value, 0.025, na.rm = TRUE)
    )

  save(
    mro = mark_recapture_object,
    file = paste(filename, "bootstrap.Rdata", sep = "")
  )

  mark_recapture_object
}
