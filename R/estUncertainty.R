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
#' @param iterations numeric. Number of bootstraps performed. Defaults to NULL.
#' @param bootstrapData data prepared by bootstrapMarkingData, offers the
#' option to compute bootstrap in parallel. Defauls to NULL.
#' @param res spatial resolution. Defaults to NULL. If NULL, resolution of the
#' markRecaptureObject$spatialResolution is used.
#' @param filename (path to file) and filename to store the markRecaptureObject
#' with bootstrap estimates.
#' @importFrom dplyr %>%
#'
#' @return  markRecaptureObject with added bootstrap uncertainty for parameters
#' @export
#' @examples mro <- estUncertainty(mro1D)
estUncertainty <- function(markRecaptureObject, parameters = c("s","m","r"),
                           iterations = NULL, bootstrapData = NULL, res = NULL,
                           filename) {

  if(is.null(bootstrapData)){
    ls <- vector(mode = "list", length = iterations)

    out <- lapply(ls, FUN = function(x){
      tmp <- bootstrapMarkingData(markRecaptureObject)
      tmp <- estParameters(tmp)
      list(s = tmp$estimates$s,
           m = tmp$estimates$m,
           r = tmp$estimates$r,
           lm = tmp$estimates$lm,
           kde = tmp$kde)
    })
  } else{
    ls <- bootstrapData

    out <- lapply(ls, FUN = function(x){
      if(is.null(res)){res <- markRecaptureObject$spatialResolution}

      tmp <- estParameters(x$data, res = res)
      list(s = tmp$estimates$s,
           m = tmp$estimates$m,
           r = tmp$estimates$r,
           lm = tmp$estimates$lm,
           kde = tmp$kde)
    })
  }

  # out2 <- reshape2::melt(out) %>%
  #   dplyr::rename(latitude = Var1,
  #          longitude = Var2,
  #          markArea = L3,
  #          parameter = L2,
  #          iteration = L1) %>%
  #   dplyr::mutate(latitude = latitude/markRecaptureObject$spatialResolution,
  #          longitude = longitude/markRecaptureObject$spatialResolution)
  #
  #  out3 <- out2 %>%
  #    dplyr::group_by(latitude, longitude, markArea, parameter) %>%
  #    dplyr::summarise(uq = quantile(value, 0.975, na.rm = TRUE),
  #             lq = quantile(value, 0.025, na.rm = TRUE))
  #
  # markRecaptureObject$estimates$bootstrap <- list(rawBootstrap = out2,
  #                                                 bootstrapQuantiles = out3)

  markRecaptureObject$estimates$bootstrap$rawBootstrap <- out
  save(mro = markRecaptureObject, file = paste(filename, ".Rdata",
                                                        sep = ""))

  return(markRecaptureObject)
}
