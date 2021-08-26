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

#' simulated data for the continuous approach (2D)
#'
#' A data set containing data simulated with constant recovery probability in
#' two-dimensional space and continuous estimates of all parameters.
#'
#' @docType data
#' @usage data(mro2D)
#' @format A markRecaptureObject:
#' \describe{
#'   \item{mro}{An object of the type markRecaptureObject containing the
#'   simulated data with the true values of the parameters and the estimated
#'   values of the continuous approach}
#' }
#'
#' @references This data set was artificially created for the CONSURE package.
#' @keywords data sets
#' @examples
#'
#' data(mro2D)
#' str(mro2D)
"mro2D"
