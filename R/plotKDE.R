#' plot kernel density estimate and true density for simulated data
#'
#' This function plots the kernel density estimate and true density for simulated data.
#' @param b index of breeding area
#' @param kde kernel density estimate from raw data
#' @param res_x resolution in x direction
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param ylim vector in the form of c(ymin,ymax): limits of the y-axis. Defaults to c(0,3).
#' @return depending on arguments plot as pdf or to plot device
#' @export
#' @examples plotKDE1D()

# plot kernel density estimate and true density
plotKDE1D <- function(b,res_x,markRecaptureObject, pdf = FALSE, ylim = c(0,1.5),dataType="sim"){

  r <- markRecaptureObject$winteringArea$recovery
  s <- markRecaptureObject$winteringArea$survival
  T <- markRecaptureObject$observationTime
  m <- markRecaptureObject$breedingAreas[[b]]$migratoryConnectivity
  p <- 1-p_nf(b,markRecaptureObject)
  xlim <- markRecaptureObject$winteringArea$window$xrange
  kde <- markRecaptureObject$kde[[dataType]]

  if(pdf) pdf("KDE.pdf")
  plot(NA, xlim = xlim, ylim = ylim,
       xlab = "wintering area w", ylab = "density", main = paste("breeding area",b))
  for(t in 1:T){
    lines(seq(xlim[1],xlim[2],length.out = res_x), colMeans(kde[[b]]$z[[t]]$v), col = t)

    lines(seq(xlim[1],xlim[2],length.out = res_x),
          f_f(seq(xlim[1],xlim[2],length.out = res_x),t,b, markRecaptureObject,p),
          lty = 2, col = t)
  }
  legend("topright",c("true","estimate"), lty = c(2,1), col = 1)
  if(pdf) dev.off()
}
