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

#' plot kernel density estimate and true density for simulated data
#'
#' This function plots the kernel density estimate and true density for
#' simulated data.
#' @inheritParams p_nf
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param ylim vector in the form of c(ymin,ymax): limits of the y-axis.
#' Defaults to c(0,3).
#' @param trueValuesAvailable logical, use TRUE for simulated data, FALSE for
#' real-world data. Defaults to FALSE.
#' @param log logical, uses log-scale for kernel-density-values if TRUE.
#' Defaults to FALSE.
#' @param ageMin numeric. Defaults to 0. If set only data points older than
#' ageMin are plotted.
#' @param ageMax numeric. If set only data points younger or equal ageMax are
#' plotted. Defaults to NULL.
#' @param drawBoundaries logical, country boundaries will be drawn, if TRUE.
#' Defaults to TRUE.
#' @param title logical, if TRUE the name of the breeding area will be
#' added as the title. If FALSE, no title will be added. Defaults to TRUE.
#' @return depending on arguments plot as pdf or to plot device
#' @importFrom rlang .data
#' @export
#' @examples plotKDE("all", mro1D, trueValuesAvailable = TRUE)
plotKDE <- function(b, markRecaptureObject, pdf = FALSE, ylim = c(0, 1.5),
                    trueValuesAvailable = FALSE,
                    log = FALSE,
                    ageMin = 0, ageMax = NULL,
                    drawBoundaries = TRUE,
                    title = TRUE) {
  main <- ""
  if (title) main <- b
  res <- markRecaptureObject$spatialResolution
  oT <- markRecaptureObject$observationTime
  xlim <- markRecaptureObject$winteringArea$window$xrange
  kde <- markRecaptureObject$kde
  dim <- markRecaptureObject$spatialDim

  if (trueValuesAvailable) {
    if (b == "all" & dim == 2) {
    } else {
      p <- 1 - p_nf(b, markRecaptureObject)
    }
  }


  if (pdf) pdf("KDE.pdf")
  if (dim == 1) {
    tmp <- reshape::melt(kde[[b]]$z)
    tmp <- dplyr::group_by(tmp, .data$value.x, .data$L1)
    tmp$L1 <- as.numeric(tmp$L1)
    tmp <- dplyr::summarise(tmp, y = mean(.data$value.value))
    colnames(tmp) <- c("x", "age", "y")
    tmp$dataType <- "estimated"

    if (trueValuesAvailable) {
      tmp2 <- expand.grid(
        x = seq(xlim[1], xlim[2], length.out = res),
        age = 1:oT
      )
      tmp2$y <- apply(
        tmp2, 1,
        function(x) {
          f_f(
            x["x"], x["age"], b, markRecaptureObject,
            p
          )
        }
      )
      tmp2$age <- tmp2$age
      tmp2$dataType <- "true"

      tmp <- rbind(tmp, tmp2)
    }

    if (is.null(ageMax)) {
      ageMax <- max(tmp["age"])
    }

    tmp <- tmp[tmp["age"] > ageMin & tmp["age"] <= ageMax, ]

    tmp$age <- as.factor(tmp$age)

    pg <- ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(
        x = .data$x,
        y = .data$y,
        col = .data$age,
        linetype = .data$dataType
      ),
      data = tmp, size = 1.5
      ) +
      ggplot2::scale_color_viridis_d(end = 0.9) +
      ggplot2::labs(
        x = "non-breeding area", y = "density",
        linetype = "datatype", color = "age", title = main
      ) +
      ggplot2::theme(text = ggplot2::element_text(size = 20))
  } else if (dim == 2) {
    ylim <- markRecaptureObject$winteringArea$window$yrange


    kdeGrid <- reshape::melt(kde[[b]]$z)[c(1:3, 5)]
    colnames(kdeGrid) <- c("longitude", "latitude", "kde", "time")
    kdeGrid$time <- as.numeric(kdeGrid$time)
    kdeGrid$dataType <- "estimated"

    if (b == "all") {
      if (trueValuesAvailable) {
        print("Not possible to plot true state for b = 'all'")
      }
    } else {
      if (trueValuesAvailable) {
        kdeGridTrue <- numeric(4)
        gridTmp <- expand.grid(
          longitude = seq(xlim[1], xlim[2],
            length.out = res
          ),
          latitude = seq(ylim[1], ylim[2], length.out = res)
        )
        for (t in 1:oT) {
          tmp <- gridTmp
          tmp$kde <- apply(gridTmp, 1, function(x) {
            f_f(x,
              t = t, b = b,
              markRecaptureObject,
              p = p
            )
          })
          tmp$time <- t
          kdeGridTrue <- rbind(kdeGridTrue, tmp)
        }
        kdeGridTrue <- kdeGridTrue[-1, ]

        kdeGridTrue$dataType <- "true"
        kdeGrid <- as.data.frame(rbind(kdeGrid, kdeGridTrue))
      }
    }

    if (is.null(ageMax)) {
      ageMax <- max(kdeGrid["time"])
    }

    kdeGrid <- kdeGrid[kdeGrid["time"] > ageMin & kdeGrid["time"] <= ageMax, ]

    if (log) {
      trans <- "log"
      my_breaks <- exp(seq(min(log(kdeGrid$kde[kdeGrid$kde != 0]),
        na.rm = TRUE
      ),
      max(kdeGrid$kde, na.rm = TRUE),
      length.out = 7
      ))[2:6]
    } else {
      trans <- "identity"
      my_breaks <- seq(min(kdeGrid$kde, na.rm = TRUE),
        max(kdeGrid$kde, na.rm = TRUE),
        length.out = 7
      )[2:6]
    }

    pg <- ggplot2::ggplot() +
      ggplot2::ggtitle(paste(b)) +
      ggplot2::scale_fill_viridis_c("kde",
        trans = trans,
        breaks = my_breaks,
        labels = formatC(my_breaks,
          format = "e",
          digits = 1
        )
      ) +
      ggplot2::theme(text = ggplot2::element_text(size = 23))

    if (!trueValuesAvailable) {
      pg <- pg + ggplot2::geom_tile(
        data = kdeGrid,
        ggplot2::aes(.data$longitude,
          .data$latitude,
          fill = .data$kde
        )
      )
    } else {
      pg <- pg + ggplot2::geom_tile(
        data = kdeGrid,
        ggplot2::aes(.data$longitude,
          .data$latitude,
          fill = .data$kde
        ),
        height = 1 / res, width = 1 / res
      )
    }

    if (drawBoundaries) {
      pg <- pg +
        ggplot2::borders("world", colour = "grey30", size = 1) +
        ggplot2::coord_sf(
          xlim = xlim,
          ylim = ylim,
          expand = FALSE
        )
    }

    if (trueValuesAvailable) {
      pg <- pg + ggplot2::facet_wrap(dataType ~ time)
    } else {
      pg <- pg + ggplot2::facet_wrap(~time)
    }
  }
  if (pdf) {
    if (pdf) plot(pg)
    grDevices::dev.off()
  }
  pg
}
