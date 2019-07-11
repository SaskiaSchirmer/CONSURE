#' plot raw recoveries of real world data
#'
#' This function plots the distribution of recoveries in the real world data in Europe and Africa.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param pdfName string to name pdf-file, please include ".pdf"!
#' @param areaNames optional, order of breeding area names to be plotted, defaults to NULL.
#' In the default case order of breeding areas in markRecaptureObject will be chosen.
#' @return depending on arguments plot as pdf or to plot to device
#' @export
#' @examples plotRawRecoveries()

plotRawRecoveries <- function(markRecaptureObject, pdf = FALSE, pdfName = "rawRecoveries.pdf",
                              areaNames = NULL){
  if(pdf) pdf(pdfName)
  par(mfrow = c(2,3), oma = c(2,1,1,1))

  if(is.null(areaNames)) areaNames <- names(markRecaptureObject$breedingAreas)

  for(i in areaNames){
    birdring::draw.map(-24, 71, -35, 71, col.land="white", col.water=grey(0.5),
             detail=FALSE, axes=FALSE)
    mtext(i)
    points(recLat ~ recLon, data = markRecaptureObject$winteringArea$data)
    points(recLat ~ recLon,
           data = markRecaptureObject$winteringArea$data[markRecaptureObject$winteringArea$data$markArea == i,],
           col = "red")
  }
  add_legend <- function(...) {
    opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0),
                mar=c(0, 0, 0, 0), new=TRUE)
    on.exit(par(opar))
    plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
    legend(...)
  }
  add_legend("bottom",legend = c("all recoveries", "recoveries of specified ringing scheme"),
             pch = 1, col = 1:2,horiz = TRUE, bty = "n")
  if(pdf) dev.off()
}
