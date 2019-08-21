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
#' @examples plotKDE()

# plot kernel density estimate and true density
plotKDE <- function(b,res_x,markRecaptureObject, pdf = FALSE, ylim = c(0,1.5),dataType="sim"){

  T <- markRecaptureObject$observationTime
  p <- 1-p_nf(b,markRecaptureObject)
  xlim <- markRecaptureObject$winteringArea$window$xrange
  kde <- markRecaptureObject$kde[[dataType]]
  dim <- markRecaptureObject$spatialDim

  if(pdf) pdf("KDE.pdf")
    if(dim == 1){
      plot(NA, xlim = xlim, ylim = ylim,
        xlab = "wintering area w", ylab = "density", main = paste("breeding area",b))
      for(t in 1:T){
          lines(seq(xlim[1],xlim[2],length.out = res_x), colMeans(kde[[b]]$z[[t]]$v), col = t)

          lines(seq(xlim[1],xlim[2],length.out = res_x),
            f_f(seq(xlim[1],xlim[2],length.out = res_x),t,b, markRecaptureObject,p),
            lty = 2, col = t)
      }
    legend("topright",c("true","estimate"), lty = c(2,1), col = 1)
    } else if(dim == 2){
      if(b == "all"){print("This plot is not working for b = 'all'")}else{
      kdeGrid <- as.data.frame(matrix(NA, ncol = 4))
      colnames(kdeGrid) <- c("longitude","latitude","kernel density estimate","time")
      for(t in 1:T){
        tmp <- reshape::melt(kde[[b]]$z[[t]]$v)
        tmp$time <- t
        colnames(tmp) <- colnames(kdeGrid)
        kdeGrid <- rbind(kdeGrid,tmp)
      }
      kdeGrid <- kdeGrid[-1,]
        pg <- ggplot2::ggplot(kdeGrid, aes(latitude, longitude, z =
                                    `kernel density estimate`,
                                  fill = `kernel density estimate`))+  ggplot2::facet_wrap(~time) +
          ggplot2::geom_tile() + ggplot2::geom_contour()+ ggplot2::ggtitle(paste("breeding area",b))
        plot(pg)
      }
    }
  if(pdf) dev.off()
}
