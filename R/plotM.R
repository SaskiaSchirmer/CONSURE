#' plot true and estimated migratory connectivity
#'
#' This function plots the kernel density estimate and true density for simulated data.
#' @param res_x resolution in x direction
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param m_fit estimate for m, matrix
#' @return depending on arguments plot as pdf or to plot device
#' @export
#' @examples plotM()
#'
plotM <- function(res_x,markRecaptureObject, m_fit, all = FALSE, pdf = FALSE){
  if(pdf) pdf("plotM.pdf")
  B <- markRecaptureObject$numberOfBreedingAreas
  xlim <- markRecaptureObject$winteringArea$window$xrange

  plot(NA, ylim = c(0,3), xlim = xlim, ylab = "density",
       xlab = "wintering area")

  if(all){
    m <- markRecaptureObject$breedingAreas[["all"]]$migratoryConnectivity
    curve(m(x), lty = 1, add = TRUE, col = "grey50")
    lines(seq(0,xlim[2],length.out = length(m_fit[["all"]])),m_fit[["all"]], lty = 1, col = "red")
  } else{
    for(b in 1:B){
      m <- markRecaptureObject$breedingAreas[[b]]$migratoryConnectivity
      curve(m(x), lty = b, add = TRUE, col = "grey50")
      lines(seq(0,xlim[2],length.out = length(m_fit[[b]])),m_fit[[b]], lty = b, col = "red")
    }
  }


  legend(xlim[2]+0.1,3/2+0.3,col = c("grey50","red"),
         legend = c( "true m", "estimated m"),
         xpd = TRUE, lty = c(3,1))

  if(pdf) dev.off()
}
