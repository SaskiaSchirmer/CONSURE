#' plot true and estimated survival function
#'
#' This function estimates the survival from a kernel density estimate of the data
#' of recovered individuals. It uses the data of all breeding areas at once.
#' @param res_x numeric. resolution in space in west-east direction.
#' @param res_y numeric. resolution in space in north-south direction. Defaults to res_x.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param dataType character, use "sim" for simulated data, "data" for real world data. Defaults to "sim".
#' @param xlim vector of lower bound and upper bound of x. Defaults to NULL.
#' @param ylim vector of lower bound and upper bound of y. Defaults to NULL.
#' @param drawBoundaries logical, country boundaries will be drawn, if TRUE. Defaults to TRUE.
#' @param xlb if not NULL, it zooms the plot to the limits given by xlim and ylim
#' @param zlim boundaries in the direction of survival values
#' @return vector of length res_x with survival probabilities dependent on space
#' @export
#' @examples plotS()


plotS <- function(res_x,res_y = res_x, markRecaptureObject,pdf = FALSE,dataType = "sim",
                  xlim = NULL, ylim = NULL,drawBoundaries = TRUE, xlb = NULL,zlim = c(0,1)) {
  s <- markRecaptureObject$winteringArea$survival
  s_fit <- markRecaptureObject$estimates$s
  dim <- markRecaptureObject$spatialDim
  xlim <- markRecaptureObject$winteringArea$window$xrange
  ylim <- markRecaptureObject$winteringArea$window$yrange
  longitude <- markRecaptureObject$kde[[dataType]]$all$z$`1`$xcol
  latitude <- markRecaptureObject$kde[[dataType]]$all$z$`1`$yrow
  if(pdf) pdf(paste("estimateS_",format(Sys.time(), "%H%M%S_%d%m%Y"),".pdf",sep = ""), width = 9, height = 6)

  if(dim == 1){
    par(mar = c(5,6,3,16)+0.1, mfrow = c(1,1))
    plot(1:res_x,s(seq(0,1,length.out = res_x)), col = "grey50", ylim = c(0,1), type="l",
         lwd = 4,

      axes = FALSE, lty = 2,ann = FALSE, frame.plot = TRUE)
    axis(1, seq(0,res_x,length.out = 6),
       seq(0,1,length.out = 6),cex.axis = 2, mgp = c(3,1,0))
    mtext(1,text = "wintering area", cex = 2, line = 3)

    axis(2, seq(0,1,length.out = 6),seq(0,1,length.out = 6),cex.axis = 2)
    mtext(2,text = "survival probability", cex = 2, line = 4)

    lines(1:res_x,s_fit, col = "red", lwd = 4)

    legend(1.1*res_x,0.8,col = c("grey50","red"),
         legend = c( "true", "estimated"),title = "survival",
         xpd = TRUE, lty = c(3,1), cex  = 2, lwd = 3)
    par(mar = c(5,4,4,10)+0.1)
  } else if(dim == 2){

    sGrid <- reshape::melt(s_fit)
    sGrid$X1 <- rep(longitude,each = res_x)
    sGrid$X2 <- rep(latitude,res_y)
    sGrid$dataType <- "estimated"
    colnames(sGrid) <- c("longitude","latitude","s","dataType")

    plotS <- ggplot2::ggplot()+
      ggplot2::geom_tile(data = sGrid, ggplot2::aes(longitude, latitude, fill = s)) +
      #ggplot2::geom_contour(data = sGrid, ggplot2::aes(longitude, latitude, z = s))+
      ggplot2::labs(fill = "estimated\n survival")+
      ggplot2::scale_fill_distiller("survival", palette = "Spectral",limits = zlim
                                    #trans = trans,# limits = c(4e-44,1),
                                    #breaks = my_breaks, labels = formatC(my_breaks,format="e",digits=1)
                                    )+
      ggplot2::theme(text = ggplot2::element_text(size = 24))
    if(drawBoundaries){
      plotS <- plotS +
        ggplot2::geom_sf(data = countryBoundaries, color = "grey30",fill = "white", size = 1) +
        ggplot2::coord_sf(xlim = xlim,
                          ylim = ylim,
                          expand = FALSE)
    }
    if(!is.null(xlb)){
      plotS <- plotS + ggplot2::coord_cartesian(xlim = xlim,ylim = ylim)


    }

    if(dataType == "sim"){
      sGridTrue <- expand.grid(longitude = seq(xlim[1],xlim[2],length.out = res_x),
                               latitude = seq(ylim[1],ylim[2],length.out = res_y))
      sGridTrue$s <- apply(sGridTrue,1,s)
      sGridTrue$dataType <- "true"
      sGrid <- as.data.frame(rbind(sGrid,sGridTrue))


      plotS <- ggplot2::ggplot()+
        ggplot2::geom_tile(data = sGrid, ggplot2::aes(longitude, latitude, fill = s),height = 1/res_y,width = 1/res_x) + # this is to fix a bug https://github.com/tidyverse/ggplot2/issues/849
        ggplot2::facet_grid(~dataType)+
        #ggplot2::geom_contour(data = sGrid, ggplot2::aes(longitude, latitude, z = s))+
        ggplot2::labs(fill = "estimated\n survival")+
        ggplot2::scale_fill_distiller("survival", palette = "Spectral",limits = zlim
                                      #trans = trans,# limits = c(4e-44,1),
                                      #breaks = my_breaks, labels = formatC(my_breaks,format="e",digits=1)
        )+
        ggplot2::theme(text = ggplot2::element_text(size = 24))

      if(drawBoundaries){
        plotS <- plotS +
          ggplot2::geom_sf(data = countryBoundaries, color = "grey30",fill = "white", size = 1) +
          ggplot2::coord_sf(xlim = xlim,
                            ylim = ylim,
                            expand = FALSE)
      }
    }
      if(pdf) plot(plotS)
  }
  if(pdf) dev.off()
  if(dim==2) plotS
}
