#' plot true and estimated survival function
#'
#' This function estimates the survival from a kernel density estimate of the data
#' of recovered individuals. It uses the data of all breeding areas at once.
#' @param res_x resolution in space
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @return vector of length res_x with survival probabilities dependent on space
#' @export
#' @examples plotS()


plotS <- function(res_x,res_y = res_x, markRecaptureObject,pdf = FALSE,dataType = "sim"){
  s <- markRecaptureObject$winteringArea$survival
  s_fit <- markRecaptureObject$estimates$s
  dim <- markRecaptureObject$spatialDim
  xlim <- markRecaptureObject$winteringArea$window$xrange
  ylim <- markRecaptureObject$winteringArea$window$yrange

  if(pdf) pdf("estimateS.pdf", width = 10,height=10)

  if(dim == 1){
    par(mar = c(5,4,4,15)+0.1, mfrow = c(1,1))
    plot(1:res_x,s(seq(0,1,length.out = res_x)), col = "grey50", ylim = c(0,1), type="l",lwd = 2,
       xlab = "wintering area", ylab = "survival probability",
       xaxt = "none", lty = 2)
    axis(1, seq(0,res_x,length.out = res_x/20),
       seq(0,1,length.out = res_x/20))

    lines(1:res_x,s_fit, col = "red")

    legend(110,0.8,col = c("grey50","red"),
         legend = c( "true s", "estimated s"),
         xpd = TRUE, lty = c(3,1))
    par(mar = c(5,4,4,10)+0.1)
  } else if(dim == 2){

    sGrid <- reshape::melt(s_fit)
    sGrid$X1 <- sGrid$X1/res_x
    sGrid$X2 <- sGrid$X2/res_y
    sGrid$dataType <- "estimated"
    colnames(sGrid) <- c("longitude","latitude","s","dataType")

    plotS <- ggplot2::ggplot(sGrid, ggplot2::aes(longitude, latitude, z =
                                         s,
                                       fill = s))+
      ggplot2::geom_tile() + ggplot2::geom_contour()+ ggplot2::labs(fill = "estimated\n survival")

    if(dataType == "sim"){
      sGridTrue <- expand.grid(longitude = seq(xlim[1],xlim[2],length.out = res_x),
                               latitude = seq(ylim[1],ylim[2],length.out = res_y))
      sGridTrue$s <- apply(sGridTrue,1,s)
      sGridTrue$dataType <- "true"
      sGrid <- as.data.frame(rbind(sGrid,sGridTrue))
    }
    plotS <- ggplot2::ggplot(sGrid, ggplot2::aes(longitude, latitude, z =
                                                   s,
                                                 fill = s))+
      ggplot2::facet_grid(~dataType)+
      ggplot2::geom_tile(height = 1/res_y,width = 1/res_x) + # this is to fix a bug https://github.com/tidyverse/ggplot2/issues/849
      ggplot2::geom_contour()+

      ggplot2::labs(fill = "estimated\n survival")
      plot(plotS)
  }
  if(pdf) dev.off()
}
