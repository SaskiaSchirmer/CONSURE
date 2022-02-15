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
#' @return  markRecaptureObject with estimates of all selected parameters
#' @export
#' @examples mro <- estParameters(mro1D)

estParameters <- function(markRecaptureObject, parameters = c("s","m","r")) {

  markRecaptureObject <- estKDE(markRecaptureObject,res_x, all = TRUE)
  markRecaptureObject <- estKDE(markRecaptureObject,res_x)

  markRecaptureObject <- estS(markRecaptureObject = markRecaptureObject)

  markRecaptureObject <- estM( markRecaptureObject =  markRecaptureObject)
  markRecaptureObject <- estM(markRecaptureObject = markRecaptureObject, all  = TRUE)

  markRecaptureObject <- estR(markRecaptureObject)

  return(markRecaptureObject)
}
