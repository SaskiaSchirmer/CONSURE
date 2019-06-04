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
plotM <- function(res_x,markRecaptureObject, m_fit){
  B <- markRecaptureObject$numberOfBreedingAreas
  xlim <- markRecaptureObject$winteringArea$window$xrange

  plot(NA, ylim = c(0,3), xlim = xlim, ylab = "density",
       xlab = "wintering area")
  for(b in 1:B){
    m <- markRecaptureObject$breedingAreas[[b]]$migratoryConnectivity
    curve(m(x), lty = b, add = TRUE, col = "grey50")
    lines(seq(1,res_x,length.out = length(m_fit[[b]])),m_fit[[b]], lty = b, col = "red")
  }
}
