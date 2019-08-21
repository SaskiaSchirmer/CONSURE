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
plotM <- function(res_x,markRecaptureObject, all = FALSE, pdf = FALSE){
  if(pdf) pdf("plotM.pdf")
  B <- markRecaptureObject$numberOfBreedingAreas
  xlim <- markRecaptureObject$winteringArea$window$xrange
  m_fit <- markRecaptureObject$estimates$m
  dim <- markRecaptureObject$spatialDim

  if(dim == 1){
    plot(NA, ylim = c(0,3), xlim = xlim, ylab = "density",
        xlab = "wintering area")

    if(all){
      m <- markRecaptureObject$breedingAreas[["all"]]$migratoryConnectivity
      curve(m(x), lty = 1, add = TRUE, col = "grey50")
      lines(seq(0,xlim[2],length.out = length(m_fit[["all"]])),m_fit[["all"]],
          lty = 1, col = "red")
    } else{
      for(b in 1:B){
        m <- markRecaptureObject$breedingAreas[[paste("b",b,sep = "")]]$migratoryConnectivity
        curve(m(x), lty = b, add = TRUE, col = "grey50")
        lines(seq(0,xlim[2],length.out = length(m_fit[[paste("b",b,sep = "")]])),
            m_fit[[paste("b",b,sep = "")]], lty = b, col = "red")
      }
    }

    legend(xlim[2]+0.1,3/2+0.3,col = c("grey50","red"),
         legend = c( "true m", "estimated m"),
         xpd = TRUE, lty = c(3,1))
    }else if(dim == 2){
      if(all){
        mGrid <- reshape::melt(m_fit)
        colnames(mGrid) <- c("longitude","latitude","mhat","breedingArea")
        mGrid <- mGrid[mGrid$breedingArea == "all",]

        pg <- ggplot2::ggplot(mGrid, ggplot2::aes(longitude, latitude, z = mhat,
                                                  fill = mhat))+
          ggplot2::facet_grid(~breedingArea)+
          ggplot2::geom_tile() +
          ggplot2::geom_contour()+
          ggplot2::labs(fill = "estimated\n migratory\n connectivity")
        plot(pg)
      }else{
        mGrid <- reshape::melt(m_fit)
        colnames(mGrid) <- c("longitude","latitude","mhat","breedingArea")
        mGrid <- mGrid[mGrid$breedingArea != "all",]

        pg <- ggplot2::ggplot(mGrid, ggplot2::aes(longitude, latitude, z = mhat,
                                                fill = mhat))+
                ggplot2::facet_grid(~breedingArea)+
                ggplot2::geom_tile() +
                ggplot2::geom_contour()+
                ggplot2::labs(fill = "estimated\n migratory\n connectivity")
        plot(pg)
      }
    }

  if(pdf) dev.off()
}
