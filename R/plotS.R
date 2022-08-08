#' Defaults to TRUE.
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

#' plot true and estimated survival function
#'
#' This function estimates the survival from a kernel density estimate of the
#' data of recovered individuals. It uses the data of all breeding areas at
#' once.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param trueValuesAvailable logical, use TRUE for simulated data, FALSE for
#' real-world data. Defaults to FALSE.
#' @param drawBoundaries logical, country boundaries will be drawn, if TRUE.
#' Only works if coordinates are in longitude/latitude system. Defaults to FALSE.
#' @param xlb if not NULL, it zooms the plot to the limits given by xlim and
#' ylim
#' @param zlim boundaries in the direction of survival values
#' @param noCI suppresses drawing the confidence interval, even if bootstrap
#' information is in the markRecaptureObject. Defaults to FALSE.
#' @param profileOfParameter sf-object containing the information of the profile
#' line, along which the values of a parameter can be plotted including the
#'  bootstrap confidence interval. If this information is given, the profile
#'  line will be plotted
#'
#' @importFrom rlang .data
#' @return vector of length res with survival probabilities dependent on space
#' @export
#' @examples plotS(mro1D, trueValuesAvailable = TRUE)
plotS <- function(markRecaptureObject, pdf = FALSE, trueValuesAvailable = FALSE,
                  drawBoundaries = FALSE, xlb = NULL,
                  zlim = c(0, 1), lon = NULL, lat = NULL,
                  noCI = FALSE, profileOfParameter = NULL) {
  res <- markRecaptureObject$spatialResolution
  s <- markRecaptureObject$winteringArea$survival
  s_fit <- markRecaptureObject$estimates$s
  dim <- markRecaptureObject$spatialDim
  xlim <- markRecaptureObject$winteringArea$window$xrange
  ylim <- markRecaptureObject$winteringArea$window$yrange
  bootstrap <- markRecaptureObject$estimates$bootstrap$bootstrapQuantiles[
    markRecaptureObject$estimates$bootstrap$bootstrapQuantiles$parameter == "s",
    ]
  crs <- markRecaptureObject$winteringArea$crs

  if(is.null(lon))  lon <- markRecaptureObject$kde$all$z$`1`$xcol
  if(is.null(lat)) lat <- markRecaptureObject$kde$all$z$`1`$yrow

  if (pdf) {
    pdf(paste("estimateS_", format(Sys.time(), "%H%M%S_%d%m%Y"), ".pdf",
      sep = ""
    ),
    width = 9, height = 6
    )
  }

  if (dim == 1) {
    dat <- data.frame(
      x = seq(xlim[1], xlim[2], length.out = res), y = c(s_fit),
      dataType = "estimated"
    )

    dat2 <- NULL
    if (trueValuesAvailable) {
      dat2 <- data.frame(x = seq(xlim[1], xlim[2], length.out = res))
      dat2$y <- s(seq(xlim[1], xlim[2], length.out = res))
      dat2$dataType <- "true"
    }
    dat <- rbind(dat, dat2)
    plotS <- ggplot2::ggplot()

    if(!is.null(bootstrap) & !noCI){
      plotS <- plotS +
        ggplot2::geom_ribbon(data = bootstrap, ggplot2::aes(x =lon,
                                                            ymin = .data$lq,
                                                            ymax = .data$uq,
                                         linetype = "variability",
                                         color = "variability"),
                    alpha = 0.7, fill = "grey")
    }

    plotS <- plotS +
      ggplot2::geom_line(ggplot2::aes(
        x = .data$x, y = .data$y,
        linetype = .data$dataType, color = .data$dataType
      ),
      data = dat, size = 1.5) +
      ggplot2::labs(
        x = "destination area", y = "survival"
      ) +
      ggplot2::theme(text = ggplot2::element_text(size = 20)) +
      ggplot2::coord_cartesian(ylim = zlim)

    if(!is.null(bootstrap) & !noCI){
      plotS <- plotS +
        ggplot2::labs(color  = "Guide name", linetype = "Guide name",
             shape = "Guide name") +
        ggplot2::scale_colour_manual("",
                            breaks = c("variability", "estimated", "true"),
                            values = c("grey", "black", "black"))  +
        ggplot2::scale_linetype_manual("",
                              breaks = c("variability", "estimated", "true"),
                              values = c(1, 1, 2))
    } else {
      plotS <- plotS +
        ggplot2::labs(color  = "Guide name", linetype = "Guide name",
             shape = "Guide name") +
        ggplot2::scale_colour_manual("",
                            breaks = c("estimated", "true"),
                            values = c("black", "black"))  +
        ggplot2::scale_linetype_manual("",
                              breaks = c("estimated", "true"),
                              values = c(1, 2))
    }

  } else if (dim == 2) {
    sGrid <- reshape::melt(s_fit)
    sGrid$X1 <- rep(lon, each = res)
    sGrid$X2 <- rep(lat, res)
    sGrid$dataType <- "estimated"
    colnames(sGrid) <- c("longitude", "latitude", "s", "dataType")

    plotS <- ggplot2::ggplot() +
      ggplot2::geom_tile(data = sGrid, ggplot2::aes(.data$longitude,
        .data$latitude,
        fill = .data$s
      )) +
      ggplot2::labs(fill = "estimated\n survival") +
      ggplot2::scale_fill_viridis_c("survival",
        limits = zlim,
        na.value = "grey90"
      ) +
      ggplot2::theme(text = ggplot2::element_text(size = 20))
    if (drawBoundaries) {
      plotS <- plotS +
        ggplot2::borders("world", colour = "grey30", size = 1) +
        ggplot2::coord_sf(
          xlim = xlim,
          ylim = ylim,
          expand = FALSE
        )
    }
    if (!is.null(xlb)) {
     # plotS <- plotS + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
    }

    if (trueValuesAvailable) {
      sGridTrue <- expand.grid(
        longitude = lon,
        latitude = lat
      )
      attr(sGridTrue, "out.attrs") <- NULL
      sGridTrue$s <- apply(sGridTrue, 1, s)
      sGridTrue$dataType <- "true"
      sGrid <- as.data.frame(rbind(sGrid, sGridTrue))

      plotS <- ggplot2::ggplot() +
        ggplot2::geom_tile(
          data = sGrid, ggplot2::aes(.data$longitude,
            .data$latitude,
            fill = .data$s
          )
        ) +
        # this is to fix a bug https://github.com/tidyverse/ggplot2/issues/849
        ggplot2::facet_grid(~dataType) +
        ggplot2::labs(fill = "estimated\n survival") +
        ggplot2::scale_fill_viridis_c("survival",
          limits = zlim,
          na.value = "grey90"
        ) +
        ggplot2::theme(text = ggplot2::element_text(size = 20))


      if (drawBoundaries) {
        plotS <- plotS +
          ggplot2::borders("world", colour = "grey30", size = 1) +
          ggplot2::coord_sf(
            xlim = xlim,
            ylim = ylim,
            expand = FALSE
          )
      }
    }

    if(!is.null(profileOfParameter)){
      plotS <- plotS + ggplot2::geom_line(
        ggplot2::aes(x = unname(sf::st_coordinates(profileOfParameter)[
                                      c(1,nrow(profileOfParameter)),1]),
                     y = unname(sf::st_coordinates(profileOfParameter)[
                                      c(1,nrow(profileOfParameter)),2])),
                     size = 2)
      }

    plotS <- plotS +
      ggplot2::coord_sf(expand = FALSE,
                        crs = sp::CRS(crs))
  }
  if (pdf) {
    plot(plotS)
    grDevices::dev.off()
  }

  plotS
}
