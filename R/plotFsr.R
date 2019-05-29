#' plot initial situation in 1D-space
#'
#' This function plots the initial functions for survival, recovery and
#' migratory connectivity in 1D-space.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param ylim vector in the form of c(ymin,ymax): limits of the y-axis. Defaults to c(0,3).
#' @return depending on arguments plot as pdf or to plot device
#' @export
#' @examples plotFsr1D()


# plot true state
plotFsr1D <- function(markRecaptureObject,pdf = FALSE,ylim = c(0,3)){
  require(truncnorm)
  B <- markRecaptureObject$numberOfBreedingAreas
  r <- markRecaptureObject$winteringArea$recovery
  s <- markRecaptureObject$winteringArea$survival
  T <- markRecaptureObject$observationTime
  xlim <- mro$winteringArea$window$xrange

  if(pdf) pdf("plotFsr.pdf")
  par(mar = c(5,4,4,11)+0.1)
  plot(NA, xlim = xlim, ylim = ylim, xlab = "wintering area w", ylab = "density")
  for(b in 1:B){
    m <- markRecaptureObject$breedingAreas[[b]]$migratoryConnectivity
    curve(m(x), lty = b, add = TRUE)
  }
  curve(s(x), add = TRUE, col = 2)
  curve((1-s(x)^T), add = TRUE, col = 4) # death probability over whole observation time
  abline(h=r(), col = 3)

  legend(1.1,2.3, lty = 1, col = c(1,4,2,3), legend = c("distribution",
                                                        "mortality\nover all\ntimesteps"
                                                        ,"survival", "recovery"),
         xpd = TRUE)

  par(mar = c(5,4,4,2)+0.1)
  if(pdf) dev.off()
}
