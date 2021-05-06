#' plot initial situation
#'
#' This function plots the initial functions for survival, recovery and
#' migratory connectivity in 1D- or 2D-space.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param ylim vector in the form of c(ymin,ymax): limits of the y-axis. Defaults to c(0,3).
#' @return depending on arguments plot as pdf or to plot device
#' @export
#' @examples plotMsr(mro1D)


# plot true state
plotMsr <- function(markRecaptureObject,pdf = FALSE,ylim = c(0,3)){
  #require(truncnorm)
  B <- markRecaptureObject$numberOfBreedingAreas
  r <- function(w){w-w+markRecaptureObject$winteringArea$recovery(w)}
  #r <- markRecaptureObject$winteringArea$recovery

  s <- markRecaptureObject$winteringArea$survival
  T <- markRecaptureObject$observationTime
  xlim <- markRecaptureObject$winteringArea$window$xrange


  if(pdf) pdf("plotMsr.pdf")

  graphics::par(mar = c(5,4,4,11)+0.1)

  #2d plot
  if(!identical(markRecaptureObject$winteringArea$window$yrange,c(0,0))){
    ylim <- markRecaptureObject$winteringArea$window$yrange
    plot(NA, xlim = xlim, ylim = ylim, xlab = "longitude", ylab = "latitude")



    x <- seq(0,1,length.out = 100)
    y <- seq(0,1,length.out = 100)

    # compute density for grid

    m_grid <- list()
    for(b in 1:B){
      m <- markRecaptureObject$breedingAreas[[b]]$migratoryConnectivity
      m_grid[[b]] <- par_grid(x, y, m, lb=0,ub=1)
      graphics::contour(x, y, m_grid[[b]], nlevels=5, main="Truncated Multivariate Normal Density",
              xlab=expression(x[1]), ylab=expression(x[2]),add = TRUE, lty = b)
    }

    s_grid <- par_grid(x,y,s)
    d_grid <- par_grid(x,y,function(w,s,T) {1-s(w)^T} ,s = s, T = 10)
    #r_grid <- par_grid(x,y,r)

    # plot density as contourplot

    graphics::contour(x, y, s_grid, nlevels=5, main="Truncated Multivariate Normal Density",
            xlab=expression(x[1]), ylab=expression(x[2]), add = TRUE, col = "red")
    graphics::contour(x, y, d_grid, nlevels=5, main="Truncated Multivariate Normal Density",
            xlab=expression(x[1]), ylab=expression(x[2]), add = TRUE, col = "blue")

    graphics::legend(xlim[2] + 0.1,diff(ylim)/2+0.25, lty = 1, col = c(1,4,2), legend = c("distribution",
                                                          "mortality\nover all\ntimesteps"
                                                          ,"survival"),
           xpd = TRUE)

  } else{
    plot(NA, xlim = xlim, ylim = ylim, xlab = "non-breeding area", ylab = "density")

    for(b in 1:B){
      m <- markRecaptureObject$breedingAreas[[b]]$migratoryConnectivity
      graphics::curve(m(x), lty = b, add = TRUE)
    }
    graphics::curve(s(x), add = TRUE, col = 2)
    graphics::curve((1-s(x)^T), add = TRUE, col = 4) # death probability over whole observation time



    graphics::curve(r(x), add = TRUE, col = 3)
    #abline(h=r(), col = 3)

    graphics::legend(1.1,2.3, lty = 1, col = c(1,4,2,3), legend = c("distribution",
                                                          "mortality\nover all\ntimesteps"
                                                          ,"survival", "recovery"),
           xpd = TRUE)
  }

  graphics::par(mar = c(5,4,4,2)+0.1)

  if(pdf) grDevices::dev.off()
}
