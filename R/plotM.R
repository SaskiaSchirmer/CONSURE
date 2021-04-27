#' plot true and estimated migratory connectivity
#'
#' This function plots the kernel density estimate and true density for simulated data.
#' @param res numeric, spatial resolution for longitude and latitude
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param b specifies breeding area for which the plot is drawn. Can be either a breedingAreaName,
#' the corresponding number of the breeding area or "all" for all breeding areas at once.
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param log plots logarithm of migratory connectivity. Defaults to FALSE.
#' @param trueValuesAvailable logical, use TRUE for simulated data, FALSE for real-world data. Defaults to FALSE.
#' @param uq upper quantile until which migratory connectivity value is plotted
#' @param drawBoundaries logical, country boundaries will be drawn, if TRUE. Defaults to TRUE.
#' @return depending on arguments plot as pdf or to plot device
#' @export
#' @examples plotM()
#'
plotM <- function(markRecaptureObject, b = "all", pdf = FALSE,log = FALSE,
                  trueValuesAvailable=FALSE,uq = 1,drawBoundaries = TRUE){
  if(pdf) pdf(paste("plotM_",format(Sys.time(), "%H%M%S_%d%m%Y"),".pdf",sep = ""), width = 17, height = 10)
  B <- markRecaptureObject$numberOfBreedingAreas
  xlim <- markRecaptureObject$winteringArea$window$xrange
  m_fit <- markRecaptureObject$estimates$m
  dim <- markRecaptureObject$spatialDim
  res <- markRecaptureObject$spatialResolution
  breedingAreaNames <- names(markRecaptureObject$breedingAreas)[names(markRecaptureObject$breedingAreas) != "all"]
  longitude <- markRecaptureObject$kde$all$z$`1`$xcol
  latitude <- markRecaptureObject$kde$all$z$`1`$yrow

  if(dim == 1){
    dat <- data.frame(x = seq(xlim[1],xlim[2],length.out = res))
    dat$y <- c(m_fit[[b]])
    dat$dataType <- "estimated"

    dat2 <- NULL
    if(trueValuesAvailable){
      dat2 <- data.frame(x = seq(xlim[1],xlim[2],length.out = res))
      dat2$y <- markRecaptureObject$breedingAreas[[b]]$migratoryConnectivity(dat2$x)
      dat2$dataType <- "true"
    }

    dat <- rbind(dat, dat2)

    plotM <- ggplot2::ggplot(ggplot2::aes(x=x,y=y,linetype = dataType), data = dat)+
      ggplot2::geom_line(size = 1.5)+
      ggplot2::labs(x = "non-breeding area", y= "migratory connectivity",
                    linetype = "datatype")+
      ggplot2::theme(text = ggplot2::element_text(size = 20))

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
        #mGrid$longitude <- mGrid$longitude/res
        #mGrid$latitude <- mGrid$latitude/res
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

        if(trueValuesAvailable){
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
        if(trueValuesAvailable) plotM <- plotM + ggplot2::facet_grid(dataType ~.)
      }

      if(!trueValuesAvailable){
        plotM <- plotM + ggplot2::geom_tile(data = mGrid, ggplot2::aes(longitude, latitude,fill = m))
      }else{
        plotM <- plotM + ggplot2::geom_tile(data = mGrid, ggplot2::aes(longitude, latitude,fill = m),
                                            height = 1/res,width = 1/res)
      }

      if(drawBoundaries){ plotM <- plotM +
        ggplot2::geom_sf(data = countryBoundaries, color = "grey30",fill = "white", size = 1) +
        ggplot2::coord_sf(xlim = xlim,ylim = ylim,expand = FALSE)
      }


    }

  if(pdf){
    plot(plotM)
    dev.off()
  }
  return(plotM)
}
