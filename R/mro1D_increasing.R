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

#' simulated data for the combined approach (1D)
#'
#' A dataset containing data simulated with increasing recovery probability in
#' one-dimensional space and continuous and combined estimates of all
#' parameters.
#'
#' @docType data
#' @usage data(mro1DIncreasing)
#' @format A markRecaptureObject.
#' \describe{
#'   \item{mro1DIncreasing}{An object of the type markRecaptureObject containing
#'   the simulated data with the true values of the parameteres and the
#'   estimated values of the continuous approach}
#'   \item{split}{numeric vector indicating for each point in space to which
#'    discrete wintering area it belongs}
#' }
#'
#' @references This data set was artificially created for the CONSURE package.
#' @keywords datasets
#' @examples
#'
#' data(mro1DIncreasing)
#' str(mro1DIncreasing)
"mro1DIncreasing"
