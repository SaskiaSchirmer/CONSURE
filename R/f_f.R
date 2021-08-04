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

#' density function for recovered individuals only
#'
#' This function sets up a valid density function for every spatio-temporal
#' point for recovered individuals by means of given survival, migratory
#' connectivity and recovery probability.
#' @inheritParams f_f_sub
#' @param p decimal: complementary probability to be not found
#' @return density of recovered individuals for the specified parameters
#' @export
#' @examples{
#'     p <- 1-p_nf(b=1,mro1D)
#'     ff <- f_f(1,1,1,mro1D,p)
#' }
f_f <- function(w, t, b, markRecaptureObject, p) {
  f_f_sub(w, t, b, markRecaptureObject) / p
}
