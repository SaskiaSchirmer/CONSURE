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

#' creates grid with function values
#'
#' This function creates a grid, where x are the row-names, y are the
#' column names and the grid-values are the function values at (x,y).
#' @param x vector of x-values
#' @param y vector of y-values
#' @param func function to create grid values
#' @param ... optional: arguments of func
#' @return length(x) x length(y) - matrix with function values evaluated at
#' (x,y)
#' @export
#' @examples par_grid(seq(0, 1, 10), seq(0, 1, 11), sum)
par_grid <- function(x, y, func, ...) {
  z <- matrix(nrow = length(x), ncol = length(y))

  for (m in seq_len(length(x))) {
    for (n in seq_len(length(y))) {
      w <- c(x[m], y[n])
      z[m, n] <- func(w, ...)
    }
  }
  return(z)
}
