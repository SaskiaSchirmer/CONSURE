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

#' plot initial situation
#'
#' This function plots the initial functions for survival, recovery and
#' migratory connectivity in 1D- or 2D-space.
#' @param mark_recapture_object object of class mark_recapture_object
#' (see mark_recapture_object())
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param ylim vector in the form of c(ymin,ymax): limits of the y-axis.
#' Defaults to c(0,3).
#' @return depending on arguments plot as pdf or to plot device
#' @export
#' @examples plot_msr(mro1D)
#'
#'
#' # plot true state
plot_msr <- function(mark_recapture_object, pdf = FALSE, ylim = c(0, 3)) {
  B <- mark_recapture_object$number_of_origins
  r <- function(w) {
    w - w + mark_recapture_object$destination$recovery(w)
  }

  s <- mark_recapture_object$destination$survival
  o_t <- mark_recapture_object$observation_time
  xlim <- mark_recapture_object$destination$window$xrange


  if (pdf) pdf("plotMsr.pdf")

  graphics::par(mar = c(5, 4, 4, 11) + 0.1)

  # 2d plot
  if (!identical(mark_recapture_object$destination$window$yrange, c(0, 0))) {
    ylim <- mark_recapture_object$destination$window$yrange
    plot(NA, xlim = xlim, ylim = ylim, xlab = "longitude", ylab = "latitude")



    x <- seq(0, 1, length.out = 100)
    y <- seq(0, 1, length.out = 100)

    # compute density for grid

    m_grid <- list()
    for (b in 1:B) {
      m <- mark_recapture_object$origins[[b]]$migratory_connectivity
      m_grid[[b]] <- par_grid(x, y, m, lb = 0, ub = 1)
      graphics::contour(x, y, m_grid[[b]],
        nlevels = 5, main = "Truncated
                        Multivariate Normal Density",
        xlab = expression(x[1]), ylab = expression(x[2]), add = TRUE, lty = b
      )
    }

    s_grid <- par_grid(x, y, s)
    d_grid <- par_grid(x, y, function(w, s, o_t) {
      1 - s(w)^o_t
    }, s = s, o_t = 10)

    # plot density as contourplot

    graphics::contour(x, y, s_grid,
      nlevels = 5, main = "Truncated Multivariate
                      Normal Density",
      xlab = expression(x[1]), ylab = expression(x[2]), add = TRUE,
      col = "red"
    )
    graphics::contour(x, y, d_grid,
      nlevels = 5, main = "Truncated Multivariate
                      Normal Density",
      xlab = expression(x[1]), ylab = expression(x[2]), add = TRUE,
      col = "blue"
    )

    graphics::legend(xlim[2] + 0.1, diff(ylim) / 2 + 0.25,
      lty = 1, col = c(1, 4, 2),
      legend = c(
        "distribution", "mortality\nover all\ntimesteps",
        "survival"
      ),
      xpd = TRUE
    )
  } else {
    plot(NA,
      xlim = xlim, ylim = ylim, xlab = "destination area",
      ylab = "density"
    )

    for (b in 1:B) {
      m <- mark_recapture_object$origins[[b]]$migratory_connectivity
      graphics::curve(m(x), lty = b, add = TRUE)
    }
    graphics::curve(s(x), add = TRUE, col = 2)
    graphics::curve((1 - s(x)^o_t), add = TRUE, col = 4) # death probability
    # over whole observation time
    graphics::curve(r(x), add = TRUE, col = 3)

    graphics::legend(1.1, 2.3,
      lty = 1, col = c(1, 4, 2, 3), legend = c(
        "distribution",
        "mortality\nover all\ntimesteps",
        "survival", "recovery"
      ),
      xpd = TRUE
    )
  }

  graphics::par(mar = c(5, 4, 4, 2) + 0.1)

  if (pdf) grDevices::dev.off()
}
