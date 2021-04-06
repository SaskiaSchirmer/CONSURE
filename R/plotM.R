#' plot true and estimated migratory connectivity
#'
#' This function plots the kernel density estimate and true density for simulated data.
#' @param res resolution in x direction
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param b specifies breeding area for which the plot is drawn. Can be either a breedingAreaName,
#' the corresponding number of the breeding area or "all" for all breeding areas at once.
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param log plots logarithm of migratory connectivity. Defaults to FALSE.
#' @param dataType character, use "sim" for simulated data, "data" for real world data. Defaults to "sim".
#' @param uq upper quantile until which migratory connectivity value is plotted
#' @param drawBoundaries logical, country boundaries will be drawn, if TRUE. Defaults to TRUE.
#' @return depending on arguments plot as pdf or to plot device
#' @export
#' @examples plotM()
#'
plotM <- function(res,markRecaptureObject, b = "all", pdf = FALSE,log = FALSE,
                  dataType = "sim",uq = 1,drawBoundaries = TRUE){
  if(pdf) pdf(paste("plotM_",format(Sys.time(), "%H%M%S_%d%m%Y"),".pdf",sep = ""), width = 17, height = 10)
  B <- markRecaptureObject$numberOfBreedingAreas
  xlim <- markRecaptureObject$winteringArea$window$xrange
  m_fit <- markRecaptureObject$estimates$m
  dim <- markRecaptureObject$spatialDim
  breedingAreaNames <- names(markRecaptureObject$breedingAreas)[names(markRecaptureObject$breedingAreas) != "all"]
  longitude <- markRecaptureObject$kde[[dataType]]$all$z$`1`$xcol
  latitude <- markRecaptureObject$kde[[dataType]]$all$z$`1`$yrow

  if(dim == 1){
    par(mar=c(5,6,3,10),cex = 3,lwd = 4)
    plot(NA, ylim = c(0,3), xlim = xlim, ylab = "migratory connectivity",
        xlab = "wintering area")

    if(b == "all"){
      if(dataType == "sim"){
        m <- markRecaptureObject$breedingAreas[["all"]]$migratoryConnectivity
        curve(m(x), lty = 1, add = TRUE, col = "grey50")
      }
      lines(seq(0,xlim[2],length.out = length(m_fit[["all"]])),m_fit[["all"]],
          lty = 1, col = "red")
    } else{
      col <- 1:length(breedingAreaNames)
      names(col) <- breedingAreaNames
      for(b in breedingAreaNames){
        if(dataType == "sim"){
          m <- markRecaptureObject$breedingAreas[[b]]$migratoryConnectivity
          curve(m(x), lty = col[b], add = TRUE, col = "grey50")
        }
        lines(seq(0,xlim[2],length.out = length(m_fit[[b]])),
            m_fit[[b]], lty = col[b], col = "red")
      }
    }

    legend(xlim[2]+0.1,3/2+0.3,col = c("grey50","red"),
         legend = c( "true", "estimated"),
         xpd = TRUE, lty = c(3,1))
    }else if(dim == 2){
      ylim <- markRecaptureObject$winteringArea$window$yrange
      if(b == "all"){
        m <- markRecaptureObject$breedingAreas[["all"]]$migratoryConnectivity
        mGrid <- reshape::melt(m_fit)
        mGrid$X1 <- rep(longitude,each=res)
        mGrid$X2 <- rep(latitude,res)
        colnames(mGrid) <- c("longitude","latitude","m","breedingArea")
        mGrid <- mGrid[mGrid$breedingArea == "all",]

        if(log){
          trans <- "log"
          my_breaks <- exp(seq(min(log(mGrid$m[mGrid$m!=0]),na.rm = TRUE),
                               max(mGrid$m,na.rm = TRUE),length.out = 5))
        }else{
          trans <- "identity"
          my_breaks <- quantile(mGrid$m,seq(0,uq,length.out = 5), na.rm = TRUE)
        }
      }else{
        mGrid <- reshape::melt(m_fit)
        mGrid$X1 <- rep(longitude,each=res)
        mGrid$X2 <- rep(latitude,res)
        colnames(mGrid) <- c("longitude","latitude","m","breedingArea")
        #mGrid$longitude <- mGrid$longitude/res_x
        #mGrid$latitude <- mGrid$latitude/res_y
        tmp <- mGrid$m
        mGrid$m <- NULL
        mGrid$m <- tmp
        mGrid <- mGrid[mGrid$breedingArea == b,]
        mGrid$dataType <- "estimated"

        if(log){
          trans <- "log"
          #my_breaks <- max(mGrid$m, na.rm = TRUE)*10^c(0,-1,-2,-3,-4)
          my_breaks <- exp(seq(min(log(mGrid$m[mGrid$m!=0]),na.rm = TRUE),
                               max(mGrid$m,na.rm = TRUE),length.out = 5))
        }else{
          trans <- "identity"
          my_breaks <- quantile(mGrid$m,seq(0,1,length.out = 5), na.rm = TRUE)
         # my_breaks <- seq(0,quantile(mGrid$m,1, na.rm = TRUE),length.out = 11))
        }

        if(dataType == "sim"){
          #m <- list()
          #for(b in breedingAreaNames){
            m <- markRecaptureObject$breedingAreas[[b]]$migratoryConnectivity
          #}

          mGridTrue <- expand.grid(longitude = seq(xlim[1],xlim[2],length.out = res),
                                   latitude = seq(ylim[1],ylim[2],length.out = res),
                                   breedingArea = breedingAreaNames)
          mGridTrue$m <- apply(mGridTrue,1,function(x){m(as.numeric(x[1:2]))})
          #mGridTrue$m <- apply(mGridTrue,1,function(x){m[[x[3]]](as.numeric(x[1:2]))})

          mGridTrue$dataType <- "true"
          mGrid <- as.data.frame(rbind(mGrid,mGridTrue))

          if(log){
            trans <- "log"
            my_breaks <- exp(seq(min(log(mGrid$m[mGrid$m!=0]),na.rm = TRUE),
                                 max(mGrid$m,na.rm = TRUE),length.out = 5))[1:5]
          }else{
            trans <- "identity"
            my_breaks <- quantile(mGrid$m,seq(0,uq,length.out = 5), na.rm = TRUE)
          }
        }
      }

      plotM <- ggplot2::ggplot()+
        # ggplot2::geom_contour(data = mGrid, ggplot2::aes(longitude, latitude, z = mhat))+
        ggplot2::labs(fill = "estimated\n migratory\n connectivity")+
        ggplot2::scale_fill_distiller("connectivity\n [quantile]", palette = "Spectral")+
                                      #values = scales::rescale(my_breaks),
                                      #trans = trans,limits =range(my_breaks))+#,
                                      #breaks = my_breaks)+#,
                                      #labels = seq(0,1,le=5))+
                                      #labels = formatC(seq(my_breaks[1],my_breaks[11],length.out = 5),format="e",digits=1))+
        ggplot2::theme(text = ggplot2::element_text(size = 24))
        #ggplot2::guides(fill = guide_coloursteps(even.steps = TRUE))

      if(b != "all"){
        plotM <- plotM + ggplot2::facet_grid(~breedingArea)
        if(dataType == "sim") plotM <- plotM + ggplot2::facet_grid(dataType ~.)
      }

      if(dataType == "data"){
        plotM <- plotM + ggplot2::geom_tile(data = mGrid, ggplot2::aes(longitude, latitude,fill = m))
      }else{
        plotM <- plotM + ggplot2::geom_tile(data = mGrid, ggplot2::aes(longitude, latitude,fill = m),
                                            height = 1/res,width = 1/res)
      }

      if(drawBoundaries){ plotM <- plotM +
        ggplot2::geom_sf(data = countryBoundaries, color = "grey30",fill = "white", size = 1) +
        ggplot2::coord_sf(xlim = xlim,ylim = ylim,expand = FALSE)
      }

      if(pdf){
        plot(plotM)
        dev.off()
      }
      return(plotM)
    }

  if(pdf) dev.off()
}
