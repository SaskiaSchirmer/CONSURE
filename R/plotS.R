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
#' @param xlim vector of lower bound and upper bound of x. Defaults to NULL.
#' @param ylim vector of lower bound and upper bound of y. Defaults to NULL.
#' @param drawBoundaries logical, country boundaries will be drawn, if TRUE.
#' Defaults to TRUE.
#' @param xlb if not NULL, it zooms the plot to the limits given by xlim and
#' ylim
#' @param zlim boundaries in the direction of survival values
#' @importFrom rlang .data
#' @return vector of length res with survival probabilities dependent on space
#' @export
#' @examples plotS(mro1D, trueValuesAvailable = TRUE)
plotS <- function(markRecaptureObject, pdf = FALSE, trueValuesAvailable = FALSE,
                  xlim = NULL, ylim = NULL, drawBoundaries = TRUE, xlb = NULL,
                  zlim = c(0, 1)) {
  res <- markRecaptureObject$spatialResolution
  s <- markRecaptureObject$winteringArea$survival
  s_fit <- markRecaptureObject$estimates$s
  dim <- markRecaptureObject$spatialDim
  xlim <- markRecaptureObject$winteringArea$window$xrange
  ylim <- markRecaptureObject$winteringArea$window$yrange
  longitude <- markRecaptureObject$kde$all$z$`1`$xcol
  latitude <- markRecaptureObject$kde$all$z$`1`$yrow
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
    plotS <- ggplot2::ggplot(ggplot2::aes(
      x = .data$x, y = .data$y,
      linetype = as.factor(.data$dataType)
    ),
    data = dat
    ) +
      ggplot2::geom_line(size = 1.5) +
      ggplot2::labs(
        x = "non-breeding area", y = "survival",
        linetype = "datatype"
      ) +
      ggplot2::theme(text = ggplot2::element_text(size = 24)) +
      ggplot2::coord_cartesian(ylim = zlim)
  } else if (dim == 2) {
    sGrid <- reshape::melt(s_fit)
    sGrid$X1 <- rep(longitude, each = res)
    sGrid$X2 <- rep(latitude, res)
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
      plotS <- plotS + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
    }

    if (trueValuesAvailable) {
      sGridTrue <- expand.grid(
        longitude = seq(xlim[1], xlim[2],
          length.out = res
        ),
        latitude = seq(ylim[1], ylim[2],
          length.out = res
        )
      )
      sGridTrue$s <- apply(sGridTrue, 1, s)
      sGridTrue$dataType <- "true"
      sGrid <- as.data.frame(rbind(sGrid, sGridTrue))

      plotS <- ggplot2::ggplot() +
        ggplot2::geom_tile(
          data = sGrid, ggplot2::aes(.data$longitude,
            .data$latitude,
            fill = .data$s
          ),
          height = 1 / res, width = 1 / res
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
  }
  if (pdf) {
    plot(plotS)
    grDevices::dev.off()
  }

  plotS
}
