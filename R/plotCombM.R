#' Plot function for true and estimated continuous, discrete and combined
#' migratory connectivity
#'
#' This function plots (in 1D) combinations of true and estimated migratory
#' connectivity values. It is possible to chose between continuous, discrete
#' and combined estimates.
#'
#' @param markRecaptureObject object of class markRecaptureObject
#' @param optimizationObject object of class optimizationObject
#' @param pdf logical, plot as pdf?, defaults to FALSE
#' @param fileName character string, only if pdf is TRUE, defaults to current
#'                 time
#' @param trueContinuous logical, plot true continuous migratory connectivity
#' @param trueDiscrete logical, plot true discrete migratory connectivity
#' @param estContinuous logical, plot estimated continuous migratory
#'                      connectivity
#' @param estDiscrete logical, plot estimated discrete migratory connectivity
#' @param estCombined logical, plot estimated combined migratory connectivity
#' @param estCorrected logical, plot estimated corrected mgiratory connectivity,
#'     defaults to FALSE
#' @param zlim limits for migratory connectivity
#' @param drawBoundaries logical, specifies if the boundaries of the world map
#'    should be drawn, defaults to FALSE
#' @param trueValuesAvailable logical, use TRUE for simulated data, FALSE for
#'                            real-world data. Defaults to FALSE.
#' @param uq upper quantile, similar to zlim?
#' @importFrom rlang .data
#'
#' @export
#' @examples{
#'     oO <- optimizationObject(markRecaptureObject = mro1DIncreasing$mro,
#'         b = "all",
#'         split = mro1DIncreasing$split,
#'         lambda  = c(.05,300))
#'
#'     plotCombM(mro1DIncreasing$mro,oO)
#' }

plotCombM <- function(markRecaptureObject,
                      optimizationObject,
                      pdf = FALSE,
                      fileName = paste("mComb_", Sys.time(), ".pdf", sep = ""),
                      trueContinuous = TRUE,
                      trueDiscrete = TRUE,
                      estContinuous = TRUE,
                      estDiscrete = FALSE,
                      estCombined = TRUE,
                      estCorrected = FALSE,
                      zlim = NULL,
                      drawBoundaries = FALSE,
                      trueValuesAvailable = FALSE,
                      uq = 1) {
  b <- optimizationObject$b
  prop <- markRecaptureObject$breedingAreas[[b]]$mDiscrete
  dim <- markRecaptureObject$spatialDim
  mCombined <- markRecaptureObject$estimates$mCombined[[b]]
  mContinuous <- markRecaptureObject$estimates$m[[b]]
  mCorrected <- markRecaptureObject$estimates$mCorrected[[b]]
  res <- markRecaptureObject$spatialResolution
  if (trueContinuous) {
    mFunc <- markRecaptureObject$breedingAreas[[b]]$migratoryConnectivity
  }
  xlim <- markRecaptureObject$winteringArea$window$xrange
  ylim <- markRecaptureObject$winteringArea$window$yrange
  yHelp <- optimizationObject$y
  split <- unname(table(optimizationObject$split))

  if (pdf) pdf(fileName, width = 5.5)

  pl <- ggplot2::ggplot()

  if (dim == 1) {
    if (trueContinuous) {
      pl <- pl + ggplot2::stat_function(
        data = data.frame(x = yHelp),
        ggplot2::aes(color = "true"),
        fun = function(x) {
          dat <- as.data.frame(matrix(x, ncol = 1))
          apply(dat, 1, mFunc)
        }
      )
    }

    if (trueDiscrete) {
      pl <- pl + ggplot2::geom_line(data.frame(
        x = yHelp,
        y = rep(prop / split, times = split)
      ),
      mapping = ggplot2::aes(x = .data$x, y = .data$y, color = "discrete true")
      )
    }

    if (estDiscrete) {
      pl <- pl

      message("Discrete estimates cannot be plotted at the moment.")
    }


    if (estContinuous) {
      pl <- pl + ggplot2::geom_line(data.frame(
        x = yHelp,
        y = c(mContinuous)
      ),
      mapping = ggplot2::aes(
        x = .data$x, y = .data$y,
        color = "continuous estimate"
      ), size = 1.5
      )
    }

    if (estCorrected) {
      pl <- pl + ggplot2::geom_line(data.frame(
        x = yHelp,
        y = c(mCorrected)
      ),
      mapping = ggplot2::aes(
        x = .data$x, y = .data$y,
        color = "corrected estimate"
      )
      )
    }

    if (estCombined) {
      pl <- pl + ggplot2::geom_line(data.frame(
        x = yHelp,
        y = c(mCombined)
      ),
      mapping = ggplot2::aes(
        x = .data$x, y = .data$y,
        color = "combined estimate"
      ),
      size = 1.5
      )
    }

    pl <- pl +
      ggplot2::ylab("migratory connectivity") +
      ggplot2::xlab("wintering area") +
      ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        text = ggplot2::element_text(size = 20)
      ) +
      ggplot2::scale_color_manual("",
        breaks = c(
          "true", "discrete true", "discrete estimate",
          "continuous estimate", "corrected estimate",
          "combined estimate"
        ),
        values = c("black", "grey", "green", "#009E73", "#E69F00", "#D55E00")
      )

    if (!is.null(zlim)) pl <- pl + ggplot2::lims(y = zlim)
  } else if (dim == 2) {
    mGrid <- reshape::melt(mCombined)
    mGrid$X1 <- rep(yHelp$latitude, res)
    mGrid$X2 <- rep(yHelp$longitude, each = res)
    mGrid$dataType <- "combined"

    tmp <- reshape::melt(mContinuous)
    tmp$X1 <- rep(yHelp$latitude, res)
    tmp$X2 <- rep(yHelp$longitude, each = res)
    tmp$dataType <- "continuous"

    mGrid <- as.data.frame(rbind(mGrid, tmp))
    mGrid$breedingArea <- b
    mGrid <- mGrid[, c(2, 1, 3, 5, 4)]

    colnames(mGrid) <- c(
      "longitude", "latitude", "m", "breedingArea",
      "dataType"
    )
    if (trueContinuous) {
      mGridTrue <- expand.grid(
        longitude = seq(xlim[1], xlim[2], length.out = res),
        latitude = seq(ylim[1], ylim[2], length.out = res),
        breedingArea = b
      )
      mGridTrue$m <- apply(mGridTrue, 1, function(x) {
        mFunc(as.numeric(x[1:2]))
      })
      mGridTrue <- mGridTrue[, c(1, 2, 4, 3)]
      mGridTrue$dataType <- "true"

      mGrid <- as.data.frame(rbind(mGrid, mGridTrue))
    }

    my_breaks <- stats::quantile(mGrid$m, seq(0, 1, length.out = 11),
      na.rm = TRUE
    )
    my_breaks <- seq(0, max(mGrid$m, na.rm = TRUE), length.out = 11)
    my_breaks <- seq(0, stats::quantile(mGrid$m, uq, na.rm = TRUE),
      length.out = 11
    )

    mGrid$dataType <- factor(mGrid$dataType, levels = c(
      "true", "continuous",
      "combined"
    ))

    pl <- pl +
      ggplot2::labs(fill = "estimated\n migratory\n connectivity") +
      ggplot2::scale_fill_viridis_c("connectivity",
        values = scales::rescale(my_breaks),
        trans = "identity", limits = c(max(0, my_breaks[1]), my_breaks[11]),
        breaks = seq(max(0, my_breaks[1]), my_breaks[11], length.out = 5),
        labels = formatC(seq(max(0, my_breaks[1]),
          my_breaks[11],
          length.out = 5
        ), format = "e", digits = 1)
      ) +
      ggplot2::theme(text = ggplot2::element_text(size = 24))

    pl <- pl + ggplot2::facet_grid(dataType ~ .)

    if (!trueValuesAvailable) {
      pl <- pl + ggplot2::geom_tile(
        data = mGrid,
        ggplot2::aes(.data$longitude,
          .data$latitude,
          fill = .data$m
        )
      )
    } else {
      pl <- pl + ggplot2::geom_tile(
        data = mGrid, ggplot2::aes(.data$longitude,
          .data$latitude,
          fill = .data$m
        ),
        height = 1 / res, width = 1 / res
      )
    }

    if (drawBoundaries) {
      pl <- pl +
        ggplot2::borders("world", colour = "grey30", size = 1) +
        ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE)
    }
  } else {
    message("plot for number of dimension unavailable")
  }

  plot(pl)
  if (pdf) grDevices::dev.off()

  return(pl)
}
