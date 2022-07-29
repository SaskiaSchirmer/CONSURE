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
#' @param parameters vector of characters specifying the parameter names
#' considered for uncertainty estimation, possible values are s for survival,
#' m for migratory connectivity and r for recovery probability. Defaults to
#' c("s","m","r").
#' @param filename (path to file) and filename to store the markRecaptureObject
#' with bootstrap estimates.
#'
#' @importFrom dplyr %>%
#'
#' @return  raw bootstrap dataframe to be used in bootstrapQuantiles
#' @export
#' @examples mro <- bootstrapQuantiles(mro1D,"s")

bootstrapQuantiles <- function(markRecaptureObject, param = c("s","m","r"),
                               filename){

  out2 <- do.call("rbind", lapply(param,
                                  function(x) getBootstrapParameters(
                                    markRecaptureObject,x)))
  markRecaptureObject$estimates$bootstrap$rawBootstrap <- out2

  markRecaptureObject$estimates$bootstrap$bootstrapQuantiles <-
    markRecaptureObject$estimates$bootstrap$rawBootstrap  %>%
    dplyr::group_by(latitude, longitude, markArea, parameter) %>%
    dplyr::summarise(uq = quantile(value, 0.975, na.rm = TRUE),
                     lq = quantile(value, 0.025, na.rm = TRUE))

  save(mro = markRecaptureObject, file = paste(filename, "bootstrap.Rdata", sep = ""))

  markRecaptureObject
}
