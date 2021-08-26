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

#' plot goodness of fit for linear model used to estimate parameter functions
#'
#' This function plots the R^2 values of the robust or ordinary linear
#' regression used to estimate survival.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param xlb numeric, lower bound of x. Defaults to NULL.
#' @param xub numeric, upper bound of x. Defaults to NULL.
#' @param ylb numeric, lower bound of y. Defaults to NULL.
#' @param yub numeric, upper bound of y. Defaults to NULL.
#' @param drawBoundaries logical, country boundaries will be drawn, if TRUE.
#' Defaults to TRUE.
#' @return matrix of dimension res*res with R^2-values for the linear model in
#' every point.
#' @export
#' @examples plotGOFofLM(mro1D)
plotGOFofLM <- function(markRecaptureObject, pdf = FALSE,
                        xlb = NULL, xub = NULL, ylb = NULL, yub = NULL,
                        drawBoundaries = TRUE) {
  gof <- markRecaptureObject$estimates$lm$all$gof
  dim <- markRecaptureObject$spatialDim
  xlim <- markRecaptureObject$winteringArea$window$xrange
  ylim <- markRecaptureObject$winteringArea$window$yrange
  longitude <- markRecaptureObject$kde$all$z$`1`$xcol
  latitude <- markRecaptureObject$kde$all$z$`1`$yrow
  res <- markRecaptureObject$spatialResolution

  if (pdf) pdf("GOFofS.pdf", width = 9, height = 6)

  if (dim == 1) {
    plotGOF <- ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(
        x = seq(xlim[1], xlim[2], length.out = res),
        y = gof
      ), size = 1.5) +
      ggplot2::ylim(0, 1) +
      ggplot2::labs(x = "non-breeding area", y = "r squared value") +
      ggplot2::theme(text = ggplot2::element_text(size = 24))
  } else if (dim == 2) {
    gofGrid <- reshape::melt(gof)
    gofGrid$X1 <- rep(longitude, each = res)
    gofGrid$X2 <- rep(latitude)
    gofGrid$dataType <- "estimated"
    colnames(gofGrid) <- c("longitude", "latitude", "gof", "dataType")

    plotGOF <- ggplot2::ggplot() +
      ggplot2::geom_tile(data = gofGrid, ggplot2::aes(.data$longitude,
        .data$latitude,
        fill = .data$gof
      )) +
      ggplot2::labs(fill = "estimated\n survival") +
      ggplot2::scale_fill_viridis_c("r squared",
        limits = c(0, 1)
      ) +
      ggplot2::theme(text = ggplot2::element_text(size = 24))
    if (drawBoundaries) {
      plotGOF <- plotGOF +
        ggplot2::borders("world", colour = "grey30", size = 1) +
        ggplot2::coord_sf(
          xlim = xlim,
          ylim = ylim,
          expand = FALSE
        )
    }
    if (!is.null(xlb)) {
      plotGOF <- plotGOF +
        ggplot2::coord_cartesian(xlim = c(xlb, xub), ylim = c(ylb, yub))
    }
  }
  if (pdf) plot(plotGOF)
  if (pdf) grDevices::dev.off()
  plotGOF
}
