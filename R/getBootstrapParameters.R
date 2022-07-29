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

#' linear model of kernel density estimate
#'
#' This function estimates the uncertainty of the parameters survival, migratory
#' connectivity and recovery probability by bootstrapping the marking data.
#' @inheritParams estS
#' @param parameter vector of characters specifying the parameter names
#' considered for uncertainty estimation, possible values are s for survival,
#' m for migratory connectivity and r for recovery probability. Defaults to
#' c("s","m","r").
#'
#' @importFrom dplyr %>%
#'
#' @return  raw bootstrap dataframe to be used in bootstrapQuantiles
#' @export
#' @examples mro <- getBootstrapParameters(mro1D,"s")

getBootstrapParameters <- function(markRecaptureObject, param){
  out2 <- markRecaptureObject$estimates$bootstrap$rawBootstrap

  if(param == "gof"){
    out2 <- out2 %>%
      purrr::map(purrr::pluck("lm")) %>%
      purrr::map(purrr::pluck("all"))
  }

  out2 <- out2 %>% purrr::map(purrr::pluck(param))

  if(param == "kde"){
    out2 <- out2 %>%
      purrr::map(function(x) x %>% purrr::map(purrr::pluck("z"))) %>%
      purrr::map(function(x) x %>% purrr::map(function(x) x %>%
                                                purrr::map(purrr::pluck("v"))))
  }

  out2 <- out2 %>%
    reshape2::melt() %>%
    dplyr::mutate(latitude = if("Var1" %in% colnames(.)) Var1 else NA,
                  longitude = if("Var2" %in% colnames(.)) Var2 else NA,
                  iteration = if("L1" %in% colnames(.)) L1 else NA,
                  markArea = if("L2" %in% colnames(.)) L2 else "all",
                  time = if("L3" %in% colnames(.)) L3 else NA,
                  parameter = param,
                  latitude = latitude/markRecaptureObject$spatialResolution,
                  longitude = longitude/markRecaptureObject$spatialResolution) %>%
    dplyr::select(latitude, longitude, value,
                  iteration, markArea, parameter, time)

  out2
}

