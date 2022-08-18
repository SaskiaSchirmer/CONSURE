#' Plot function for true and estimated continuous, discrete and combined
#' recovery probability
#'
#' This function plots (in 1D) combinations of true and estimated recovery
#' probability values. It is possible to chose between continuous, discrete and
#' combined estimates.
#'
#' @param markRecaptureObject object of class markRecaptureObject
#' @param optimizationObject object of class optimizationObject
#' @param pdf logical, plot as pdf?, defaults to FALSE
#' @param fileName character string, only if pdf is TRUE, defaults to current
#'                 time
#' @param trueContinuous logical, plot true continuous recovery probability
#' @param trueDiscrete logical, plot true discrete recovery probability
#' @param estContinuous logical, plot estimated continuous recovery probability
#' @param estDiscrete logical, plot estimated discrete recovery probability
#' @param estCombined logical, plot estimated combined recovery probability
#' @param drawBoundaries logical, specifies if the boundaries of the world map
#'                       should be drawn, defaults to FALSE
#' @param trueValuesAvailable logical, use TRUE for simulated data, FALSE for
#'                            real-world data. Defaults to FALSE.
#' @importFrom rlang .data
#'
#' @export
#' @examples{
#'     oO <- optimizationObject(markRecaptureObject = mro1DIncreasing$mro,
#'         b = "all",
#'         split = mro1DIncreasing$split,
#'         lambda  = c(.05,300))
#'
#'     plotCombR(mro1DIncreasing$mro,oO)
#' }
plotCombR <- function(markRecaptureObject,
                      optimizationObject,
                      pdf = FALSE,
                      fileName = paste("rComb_", Sys.time(), ".pdf", sep = ""),
                      trueContinuous = TRUE,
                      trueDiscrete = TRUE,
                      estContinuous = TRUE,
                      estDiscrete = FALSE,
                      estCombined = TRUE,
                      drawBoundaries = FALSE,
                      trueValuesAvailable = FALSE) {
  b <- optimizationObject$b
  dim <- markRecaptureObject$spatialDim
  rCombined <- markRecaptureObject$estimates$rCombined
  rContinuous <- markRecaptureObject$estimates$r
  res <- markRecaptureObject$spatialResolution
  if (trueContinuous) rFunc <- markRecaptureObject$winteringArea$recovery
  xlim <- markRecaptureObject$winteringArea$window$xrange
  ylim <- markRecaptureObject$winteringArea$window$yrange
  yHelp <- optimizationObject$y

  if (pdf) pdf(fileName, width = 5.5)

  pl <- ggplot2::ggplot()

  if (dim == 1) {
    if (trueContinuous) {
      pl <- pl + ggplot2::stat_function(
        data = data.frame(x = seq(0, 1, le = 100)),
        ggplot2::aes(color = "true"),
        fun = function(x) rFunc(x)
      )
    }

    if (trueDiscrete) message("True discrete values cannot be plotted in the
                             moment.")

    if (estDiscrete) message("Estimated discrete values cannot be plotted in the
                             moment.")


    if (estCombined) {
      pl <- pl + ggplot2::geom_line(data.frame(
        x = yHelp,
        y = c(rCombined)
      ),
      mapping = ggplot2::aes(
        x = .data$x, y = .data$y,
        color = "combined estimate"
      ),
      size = 1.5
      )
    }

    if (estContinuous) {
      pl <- pl + ggplot2::geom_hline(ggplot2::aes(
        yintercept = rContinuous,
        color = "continuous estimate"
      ),
      size = 1.5
      )
    }

    pl <- pl +
      ggplot2::ylab("recovery probability") +
      ggplot2::xlab("wintering area") +
      ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        text = ggplot2::element_text(size = 20)
      ) +
      ggplot2::scale_color_manual("",
        breaks = c(
          "true", # "discrete true",
          # "discrete estimate",
          "continuous estimate", "combined estimate"
        ),
        values = c(
          "black", # "grey",
          # "green",
          "#009E73", "#E69F00"
        )
      )
  } else if (dim == 2) {
    rGrid <- reshape::melt(rCombined)
    rGrid$X1 <- rep(yHelp$longitude, each = res)
    rGrid$X2 <- rep(yHelp$latitude, res)
    rGrid$dataType <- "combined"

    tmp <- matrix(rep(rContinuous, res * res))
    tmp[!markRecaptureObject$inside] <- NA
    tmp <- as.data.frame(tmp)
    tmp$X1 <- rep(yHelp$longitude, each = res)
    tmp$X2 <- rep(yHelp$latitude, res)
    tmp$dataType <- "continuous"
    colnames(tmp) <- c("value", "X1", "X2", "dataType")
    tmp <- tmp[, c(2, 3, 1, 4)]

    rGrid <- as.data.frame(rbind(rGrid, tmp))
    rGrid$breedingArea <- b
    rGrid <- rGrid[, c(1, 2, 3, 5, 4)]

    colnames(rGrid) <- c("longitude", "latitude", "r", "breedingArea",
                         "dataType")
    if (trueContinuous) {
      rGridTrue <- expand.grid(
        longitude = seq(xlim[1], xlim[2], length.out = res),
        latitude = seq(ylim[1], ylim[2], length.out = res),
        breedingArea = b
      )
      rGridTrue$r <- apply(rGridTrue, 1, function(x) {
        rFunc(as.numeric(x[1:2]))
      })
      rGridTrue <- rGridTrue[, c(1, 2, 4, 3)]
      rGridTrue$dataType <- "true"

      rGrid <- as.data.frame(rbind(rGrid, rGridTrue))
      rGrid$dataType <- factor(rGrid$dataType,
        levels = c("true", "continuous", "combined")
      )
    } else {
      rGrid$dataType <- factor(rGrid$dataType,
        levels = c("continuous", "combined")
      )
    }

    my_breaks <- stats::quantile(rGrid$r, seq(0, 1, length.out = 11),
      na.rm = TRUE
    )
    my_breaks <- seq(0, stats::quantile(rGrid$r, 1, na.rm = TRUE),
      length.out = 11
    )


    pl <- pl +
      ggplot2::labs(fill = "estimated\n recovery\n probability") +
      ggplot2::scale_fill_viridis_c("recovery",
        values = scales::rescale(my_breaks),
        trans = "identity", limits = range(my_breaks),
        breaks = seq(my_breaks[1], my_breaks[11], length.out = 5),
        labels = formatC(seq(my_breaks[1], my_breaks[11], length.out = 5),
          format = "e", digits = 1
        )
      ) +
      ggplot2::theme(text = ggplot2::element_text(size = 24))

    pl <- pl + ggplot2::facet_grid(dataType ~ .)

    if (!trueValuesAvailable) {
      pl <- pl + ggplot2::geom_tile(data = rGrid, ggplot2::aes(.data$longitude,
        .data$latitude,
        fill = .data$r
      ))
    } else {
      pl <- pl + ggplot2::geom_tile(
        data = rGrid, ggplot2::aes(.data$longitude,
          .data$latitude,
          fill = .data$r
        ),
        height = 1 / res, width = 1 / res
      )
    }

    if (drawBoundaries) {
      pl <- pl +
        ggplot2::borders("world", colour = "grey30", size = 1) +
        ggplot2::coord_sf(
          xlim = xlim, ylim = ylim, expand = FALSE,
          crs = sp::CRS("+proj=utm +zone=31N +datum=WGS84")
        )
    }
  } else {
    message("plot for number of dimension unavailable")
  }


  plot(pl)
  if (pdf) grDevices::dev.off()

  return(pl)
}
