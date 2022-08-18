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

#' Extracting bootstrapped parameters.
#'
#' This function estimates the uncertainty of the parameters survival, migratory
#' connectivity and recovery probability by bootstrapping the marking data.
#' @inheritParams est_s
#' @param param vector of characters specifying the parameter names
#' considered for uncertainty estimation, possible values are s for survival,
#' m for migratory connectivity and r for recovery probability. Defaults to
#' c("s","m","r").
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#'
#' @return  raw bootstrap dataframe to be used in bootstrap_quantiles
#' @export
#' @examples{
#'   mro <- est_uncertainty(mro1D, "s", iterations = 2, filename = "test")
#'   extracted_parameter <- get_bootstrap_parameters(mro, "s")
#' }
#'
get_bootstrap_parameters <- function(mark_recapture_object, param) {
  out2 <- mark_recapture_object$estimates$bootstrap$raw_bootstrap

  if (param == "gof") {
    out2 <- out2 %>%
      purrr::map(purrr::pluck("lm")) %>%
      purrr::map(purrr::pluck("all"))
  }

  out2 <- out2 %>% purrr::map(purrr::pluck(param))

  if (param == "kde") {
    out2 <- out2 %>%
      purrr::map(function(x) {
        x %>%
          purrr::map(purrr::pluck("z"))
      }) %>%
      purrr::map(function(x) {
        x %>%
          purrr::map(function(x) {
            x %>%
              purrr::map(purrr::pluck("v"))
          })
      })
  }

  . <- NULL

  out2 <- out2 %>%
    reshape2::melt() %>%
    dplyr::mutate(
      latitude = if ("Var1" %in% colnames(.)) .data$Var1 else NA,
      longitude = if ("Var2" %in% colnames(.)) .data$Var2 else NA,
      iteration = if ("L1" %in% colnames(.)) .data$L1 else NA,
      mark_area = if ("L2" %in% colnames(.)) .data$L2 else "all",
      time = if ("L3" %in% colnames(.)) .data$L3 else NA,
      parameter = param,
      latitude = .data$latitude / mark_recapture_object$spatial_resolution,
      longitude = .data$longitude / mark_recapture_object$spatial_resolution
    ) %>%
    dplyr::select(
      .data$latitude, .data$longitude, .data$value,
      .data$iteration, .data$mark_area, .data$parameter, .data$time
    )

  out2
}
