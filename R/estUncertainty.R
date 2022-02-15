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
#' @param iterations numeric. Number of bootstraps performed. Defaults to 100.
#' @return  markRecaptureObject with added bootstrap uncertainty for parameters
#' @export
#' @examples mro <- estUncertainty(mro1D)
estUncertainty <- function(markRecaptureObject, parameters = c("s","m","r"),
                  iterations = 100) {

  ls <- vector(mode = "list", length = iterations)
  out <- lapply(ls, FUN = function(x){
    tmp <- bootstrapMarkingData(markRecaptureObject)
    tmp <- estParameters(tmp)
    list(s = tmp$estimates$s,
         m = tmp$estimates$m,
         r = tmp$estimates$r)
  })

  out2 <- reshape2::melt(out) %>%
    rename(latitude = Var1,
           longitude = Var2,
           markArea = L3,
           parameter = L2,
           iteration = L1) %>%
    group_by(latitude, longitude, markArea, parameter) %>%
    mutate(uq = quantile(value, 0.975), lq = quantile(value, 0.025))

  markRecaptureObject$estimates$bootstrap <- out2


  return(markRecaptureObject)
}
