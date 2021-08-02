#' plot raw recoveries of real world data
#'
#' This function plots the distribution of recoveries in the real world data in
#' Europe and Africa.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param pdfName string to name pdf-file, please include ".pdf"!
#' @param areaNames optional, order of breeding area names to be plotted,
#' defaults to NULL.
#' In the default case order of breeding areas in markRecaptureObject will be
#' chosen.
#' @param facetByAge logical, determines if the data should be faceted by the
#' age column. Defaults to FALSE.
#' @param facetByArea logical, determines if the data should be faceted by
#' the area column.
#' Defaults to FALSE.
#' @param ageMin numeric. Defaults to 0. If set only data points older than
#' ageMin are plotted.
#' @param ageMax numeric. If set only data points younger or equal ageMax are
#' plotted. Defaults to NULL.
#' @param xname character, name of column containing first dimension, defaults
#' to "longitude".
#' @param yname character, name of column containing second dimension, defaults
#' to "latitude".
#' @param timename character, name of column containing third dimension,
#' defaults to "age".
#' @param markAreaName character, name of column containing information on
#' areas where marking took place, defaults to "markArea".
#' @param plotTitle character, title of plot, defaults to "".
#' @return depending on arguments plot as pdf or to plot to device
#' @export
#' @examples plotRawRecoveries(mro1D)
plotRawRecoveries <- function(markRecaptureObject, pdf = FALSE,
                              pdfName = "rawRecoveries.pdf",
                              areaNames = NULL, facetByAge = FALSE,
                              facetByArea = FALSE,
                              ageMin = 0, ageMax = NULL,
                              xname = "longitude", yname = "latitude",
                              timename = "age", markAreaName = "markArea",
                              plotTitle = "") {
  dim <- markRecaptureObject$spatialDim

  left <- markRecaptureObject$winteringArea$window$xrange[1]
  right <- markRecaptureObject$winteringArea$window$xrange[2]
  bottom <- markRecaptureObject$winteringArea$window$yrange[1]
  top <- markRecaptureObject$winteringArea$window$yrange[2]

  if (pdf) pdf(pdfName)

  if (dim == 1) {
    dat <- do.call("rbind", markRecaptureObject$winteringArea$recoveryData)

    pl <- ggplot2::ggplot(ggplot2::aes_string(xname, timename),
      data = dat
    ) +
      ggplot2::labs(x = "non-breeding area", y = "age", title = plotTitle) +
      ggplot2::geom_point(shape = 3)

    if (facetByArea) {
      pl <- pl +
        ggplot2::facet_grid(stats::reformulate(".", markAreaName))
    }
    if (!facetByAge) message("Not facetting by age creates no meaningful plot.
                            Facetting by age anyways.")
  } else if (dim == 2) {
    if (is.null(areaNames)) {
      areaNames <- names(markRecaptureObject$winteringArea$recoveryData)
    }

    myMap <- ggmap::get_stamenmap(
      bbox = c(
        left = left, bottom = bottom,
        right = right, top = top
      ),
      zoom = 3, maptype = "terrain-background"
    )

    dat <- do.call("rbind", markRecaptureObject$winteringArea$recoveryData)

    if (is.null(ageMax)) {
      ageMax <- max(dat[timename])
    }

    dat <- dat[dat[timename] > ageMin & dat[timename] <= ageMax &
      as.character(unlist(dat[markAreaName])) %in% areaNames, ]
    dat[markAreaName] <- factor(unlist(dat[markAreaName]), levels = areaNames)

    pl <- ggmap::ggmap(myMap) +
      ggplot2::geom_point(
        data = dat, ggplot2::aes_string(x = xname, y = yname),
        size = 2
      ) +
      ggplot2::labs(x = "longitude", y = "latitude", title = plotTitle) +
      ggplot2::theme(text = ggplot2::element_text(size = 24))
    if (facetByAge) {
      if (facetByArea) {
        pl <- pl +
          ggplot2::facet_grid(stats::reformulate(timename, markAreaName))
      } else {
        pl <- pl + ggplot2::facet_grid(stats::reformulate(timename, "."))
      }
    } else if (facetByArea) {
      pl <- pl +
        ggplot2::facet_grid(stats::reformulate(".", markAreaName))
    }
  }

  if (pdf) {
    plot(pl)
    grDevices::dev.off()
  }
  pl
}
