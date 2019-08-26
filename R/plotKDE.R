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
  if(dataType == "sim"){p <- 1-p_nf(b,markRecaptureObject)}
  xlim <- markRecaptureObject$winteringArea$window$xrange
  kde <- markRecaptureObject$kde[[dataType]]
  dim <- markRecaptureObject$spatialDim

  if(pdf) pdf("KDE.pdf")
    if(dim == 1){
      plot(NA, xlim = xlim, ylim = ylim,
        xlab = "wintering area w", ylab = "density", main = paste("breeding area",b))
      for(t in 1:T){
          lines(seq(xlim[1],xlim[2],length.out = res_x), colMeans(kde[[b]]$z[[t]]$v), col = t)

          if(dataType == "sim"){
            lines(seq(xlim[1],xlim[2],length.out = res_x),
            f_f(seq(xlim[1],xlim[2],length.out = res_x),t,b, markRecaptureObject,p),
            lty = 2, col = t)
          }
      }
    legend("topright",c("true","estimate"), lty = c(2,1), col = 1)
    } else if(dim == 2){
      if(b == "all"){print("This plot is not working for b = 'all'")}else{
        kdeGrid <- reshape::melt(kde[[b]]$z)[c(1:3,5)]
        colnames(kdeGrid) <- c("longitude","latitude","kde","time")
        kdeGrid$dataType <- "estimated"

        if(dataType == "sim"){
          kdeGridTrue <- numeric(4)
          gridTmp <- expand.grid(longitude = seq(xlim[1],xlim[2],length.out = res_x),
                             latitude = seq(ylim[1],ylim[2],length.out = res_y))
          for(t in 1:T){
            tmp <- gridTmp
            tmp$kde <- apply(gridTmp,1,function(x){f_f(x,t = t, b = b,markRecaptureObject,p = p)})
            tmp$time <- t
            kdeGridTrue <- rbind(kdeGridTrue,tmp)
          }
          kdeGridTrue <- kdeGridTrue[-1,]

          kdeGridTrue$dataType <- "true"
          kdeGrid <- as.data.frame(rbind(kdeGrid,kdeGridTrue))

          pg <- ggplot2::ggplot(kdeGrid, ggplot2::aes(longitude, latitude, z =
                                                        kde,
                                                      fill = kde))+  ggplot2::facet_grid(dataType~as.numeric(time)) +
            ggplot2::geom_tile(height = 1/res_y, width = 1/res_x) +
            ggplot2::geom_contour()+
            ggplot2::ggtitle(paste("breeding area",b))
        } else{

        pg <- ggplot2::ggplot(kdeGrid, ggplot2::aes(longitude, latitude, z =
                                    kde,
                                  fill = kde))+  ggplot2::facet_wrap(~as.numeric(time)) +
          ggplot2::geom_tile() +
          ggplot2::geom_contour()+
          ggplot2::ggtitle(paste("breeding area",b))
        }
        plot(pg)
      }
    }
  if(pdf) dev.off()
}
