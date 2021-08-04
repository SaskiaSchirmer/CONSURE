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

#' plot true and estimated migratory connectivity
#'
#' This function plots the kernel density estimate and true density for
#' simulated data.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param b specifies breeding area for which the plot is drawn. Can be either
#' a breedingAreaName, the corresponding number of the breeding area or "all"
#' for all breeding areas at once.
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param log plots logarithm of migratory connectivity. Defaults to FALSE.
#' @param trueValuesAvailable logical, use TRUE for simulated data, FALSE for
#' real-world data. Defaults to FALSE.
#' @param uq upper quantile until which migratory connectivity value is plotted
#' @param drawBoundaries logical, country boundaries will be drawn, if TRUE.
#' Defaults to TRUE.
#' @importFrom rlang .data
#' @return depending on arguments plot as pdf or to plot device
#' @export
#' @examples plotM(mro1D, trueValuesAvailable = TRUE)
plotM <- function(markRecaptureObject, b = "all", pdf = FALSE, log = FALSE,
                  trueValuesAvailable = FALSE, uq = 1, drawBoundaries = TRUE) {
  if (pdf) {
    pdf(paste("plotM_", format(Sys.time(), "%H%M%S_%d%m%Y"), ".pdf", sep = ""),
      width = 17, height = 10
    )
  }
  xlim <- markRecaptureObject$winteringArea$window$xrange
  m_fit <- markRecaptureObject$estimates$m
  dim <- markRecaptureObject$spatialDim
  res <- markRecaptureObject$spatialResolution
  breedingAreaNames <- names(markRecaptureObject$breedingAreas)[
    names(markRecaptureObject$breedingAreas) != "all"
  ]
  longitude <- markRecaptureObject$kde$all$z$`1`$xcol
  latitude <- markRecaptureObject$kde$all$z$`1`$yrow

  if (dim == 1) {
    dat <- data.frame(x = seq(xlim[1], xlim[2], length.out = res))
    dat$y <- c(m_fit[[b]])
    dat$dataType <- "estimated"

    dat2 <- NULL
    if (trueValuesAvailable) {
      dat2 <- data.frame(x = seq(xlim[1], xlim[2], length.out = res))
      dat2$y <-
        markRecaptureObject$breedingAreas[[b]]$migratoryConnectivity(dat2$x)
      dat2$dataType <- "true"
    }

    dat <- rbind(dat, dat2)

    plotM <- ggplot2::ggplot(ggplot2::aes(
      x = .data$x, y = .data$y,
      linetype = .data$dataType
    ),
    data = dat
    ) +
      ggplot2::geom_line(size = 1.5) +
      ggplot2::labs(
        x = "non-breeding area", y = "migratory connectivity",
        linetype = "datatype"
      ) +
      ggplot2::theme(text = ggplot2::element_text(size = 20))
  } else if (dim == 2) {
    ylim <- markRecaptureObject$winteringArea$window$yrange
    if (b == "all") {
      m <- markRecaptureObject$breedingAreas[["all"]]$migratoryConnectivity
      mGrid <- reshape::melt(m_fit)
      mGrid$X1 <- rep(longitude, each = res)
      mGrid$X2 <- rep(latitude, res)
      colnames(mGrid) <- c("longitude", "latitude", "m", "breedingArea")
      mGrid <- mGrid[mGrid$breedingArea == "all", ]
    } else {
      mGrid <- reshape::melt(m_fit)
      mGrid$X1 <- rep(longitude, each = res)
      mGrid$X2 <- rep(latitude, res)
      colnames(mGrid) <- c("longitude", "latitude", "m", "breedingArea")
      tmp <- mGrid$m
      mGrid$m <- NULL
      mGrid$m <- tmp
      mGrid <- mGrid[mGrid$breedingArea == b, ]
      mGrid$dataType <- "estimated"

      if (trueValuesAvailable) {
        m <- markRecaptureObject$breedingAreas[[b]]$migratoryConnectivity

        mGridTrue <- expand.grid(
          longitude = seq(xlim[1], xlim[2],
            length.out = res
          ),
          latitude = seq(ylim[1], ylim[2],
            length.out = res
          ),
          breedingArea = breedingAreaNames
        )
        mGridTrue$m <- apply(mGridTrue, 1, function(x) {
          m(as.numeric(x[1:2]))
        })

        mGridTrue$dataType <- "true"
        mGrid <- as.data.frame(rbind(mGrid, mGridTrue))
      }
    }

    plotM <- ggplot2::ggplot() +
      ggplot2::labs(fill = "estimated\n migratory\n connectivity") +
      ggplot2::scale_fill_viridis_c("connectivity", na.value = "grey90") +
      ggplot2::theme(text = ggplot2::element_text(size = 24))

    if (b != "all") {
      plotM <- plotM + ggplot2::facet_grid(~breedingArea)
      if (trueValuesAvailable) {
        plotM <- plotM +
          ggplot2::facet_grid(dataType ~ .)
      }
    }

    if (!trueValuesAvailable) {
      plotM <- plotM + ggplot2::geom_tile(
        data = mGrid,
        ggplot2::aes(.data$longitude,
          .data$latitude,
          fill = .data$m
        )
      )
    } else {
      plotM <- plotM + ggplot2::geom_tile(
        data = mGrid,
        ggplot2::aes(.data$longitude,
          .data$latitude,
          fill = .data$m
        ),
        height = 1 / res, width = 1 / res
      )
    }

    if (drawBoundaries) {
      plotM <- plotM +
        ggplot2::borders("world", colour = "grey30", size = 1) +
        ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE)
    }
  }

  if (pdf) {
    plot(plotM)
    grDevices::dev.off()
  }

  return(plotM)
}
