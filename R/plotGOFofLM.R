#' plot goodness of fit for linear model used to estimate parameter functions
#'
#' This function plots the R^2 values of the robust or ordinary linear regression
#' used to estimate survival.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param xlim vector of lower bound and upper bound of x. Defaults to NULL.
#' @param ylim vector of lower bound and upper bound of y. Defaults to NULL.
#' @param drawBoundaries logical, country boundaries will be drawn, if TRUE. Defaults to TRUE.
#' @return matrix of dimension res*res with R^2-values for the linear model in every point.
#' @export
#' @examples plotGOFofLM()


plotGOFofLM <- function(markRecaptureObject,pdf = FALSE,
                  xlb = NULL, xub = NULL, ylb = NULL, yub = NULL,drawBoundaries = TRUE) {
  gof <- markRecaptureObject$estimates$lm$all$gof
  dim <- markRecaptureObject$spatialDim
  xlim <- markRecaptureObject$winteringArea$window$xrange
  ylim <- markRecaptureObject$winteringArea$window$yrange
  longitude <- markRecaptureObject$kde$all$z$`1`$xcol
  latitude <- markRecaptureObject$kde$all$z$`1`$yrow
  res <- markRecaptureObject$spatialResolution

  if(pdf) pdf("GOFofS.pdf", width = 9,height=6)

  if(dim == 1){
    par(mar = c(5,6,3,16)+0.1, mfrow = c(1,1))
    plot(1:res,gof, col = "grey50", ylim = c(0,1), type="l",
         lwd = 4,

         axes = FALSE, lty = 2,ann = FALSE, frame.plot = TRUE)
    axis(1, seq(0,res,length.out = 6),
         seq(0,1,length.out = 6),cex.axis = 2, mgp = c(3,1,0))
    mtext(1,text = "wintering area", cex = 2, line = 3)

    axis(2, seq(0,1,length.out = 6),seq(0,1,length.out = 6),cex.axis = 2)
    mtext(2,text = "r squared value", cex = 2, line = 4)


    legend(1.1*res,0.8,col = c("grey50","red"),
           legend = c( "true", "estimated"),title = "survival",
           xpd = TRUE, lty = c(3,1), cex  = 2, lwd = 3)
    par(mar = c(5,4,4,10)+0.1)
  } else if(dim == 2){

    gofGrid <- reshape::melt(gof)
    gofGrid$X1 <- rep(longitude,res)
    gofGrid$X2 <- rep(latitude,each = res)
    gofGrid$dataType <- "estimated"
    colnames(gofGrid) <- c("longitude","latitude","gof","dataType")

    plotGOF <- ggplot2::ggplot()+
      ggplot2::geom_tile(data = gofGrid, ggplot2::aes(longitude, latitude, fill = gof)) +
      #ggplot2::geom_contour(data = sGrid, ggplot2::aes(longitude, latitude, z = s))+
      ggplot2::labs(fill = "estimated\n survival")+
      ggplot2::scale_fill_viridis_c("r squared",
                                    limits = c(0,1)
                                    #trans = trans,# limits = c(4e-44,1),
                                    #breaks = my_breaks, labels = formatC(my_breaks,format="e",digits=1)
      )+
      ggplot2::theme(text = ggplot2::element_text(size = 24))
    if(drawBoundaries){
      plotGOF <- plotGOF +
        ggplot2::geom_sf(data = countryBoundaries, color = "grey30",fill = "white", size = 1) +
        ggplot2::coord_sf(xlim = xlim,
                          ylim = ylim,
                          expand = FALSE)
    }
    if(!is.null(xlb)){
      plotGOF <- plotGOF + ggplot2::coord_cartesian(xlim = c(xlb,xub),ylim = c(ylb,yub))


    }


    if(pdf) plot(plotGOF)
  }
  if(pdf) dev.off()
  if(dim==2) plotGOF
}
