#' plot kernel density estimate and true density for simulated data
#'
#' This function plots the kernel density estimate and true density for simulated data.
#' @inheritParams p_nf
#' @param res resolution for longitude and latitude
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param ylim vector in the form of c(ymin,ymax): limits of the y-axis. Defaults to c(0,3).
#' @param dataType character, use "sim" for simulated data, "data" for real world data. Defaults to "sim".
#' @param log logical, uses log-scale for kernel-density-values if TRUE. Defaults to FALSE.
#' @param facetByTime logical, plots one plot for every age class if TRUE. Defaults to TRUE.
#' @param drawBoundaries logical, country boundaries will be drawn, if TRUE. Defaults to TRUE.
#' @return depending on arguments plot as pdf or to plot device
#' @export
#' @examples plotKDE()

# plot kernel density estimate and true density
plotKDE <- function(b,res,markRecaptureObject, pdf = FALSE, ylim = c(0,1.5),dataType="sim",
                    log=FALSE, facetByTime = TRUE,drawBoundaries = TRUE){

  T <- markRecaptureObject$observationTime
  xlim <- markRecaptureObject$winteringArea$window$xrange
  kde <- markRecaptureObject$kde[[dataType]]
  dim <- markRecaptureObject$spatialDim
  if(dataType == "sim"){if(b=="all"&dim==2){}else{p <- 1-p_nf(b,markRecaptureObject)}}


  if(pdf) pdf("KDE.pdf")
    if(dim == 1){
      plot(NA, xlim = xlim, ylim = ylim,
        axes =FALSE, frame.plot = TRUE, ann = FALSE)
      title(main=b,cex.main=4)
      axis(1,cex.lab=4,cex.axis = 4,mgp = c(3,3,0), at = seq(0,1,by=0.2),
           labels = seq(0,1,by=0.2))
      mtext(1, text = "wintering area w", line = 6, cex = 3)
      axis(2,cex.lab=4,cex.axis = 4,mgp = c(3,2,0), at = seq(0,1.5,by=0.5),
           labels = seq(0,1.5,by=0.5))
      mtext(2, text = "density", line = 5, cex = 3)
      for(t in 1:T){
          lines(seq(xlim[1],xlim[2],length.out = res),
               colMeans(kde[[b]]$z[[t]]$v), col = t,lwd=4)

          if(dataType == "sim"){
            lines(seq(xlim[1],xlim[2],length.out = res),
            f_f(seq(xlim[1],xlim[2],length.out = res),t,b, markRecaptureObject,p),
            lty = 2, col = t,
            lwd = 4)
          }
      }
    #legend("topright",c("true","estimate"), lty = c(2,1), col = 1,cex = 4,lwd = 4)
    } else if(dim == 2){
      ylim <- markRecaptureObject$winteringArea$window$yrange


      if(facetByTime){
        kdeGrid <- reshape::melt(kde[[b]]$z)[c(1:3,5)]
        colnames(kdeGrid) <- c("longitude","latitude","kde","time")
        kdeGrid$time <- as.numeric(kdeGrid$time)
        kdeGrid$dataType <- "estimated"
      }else{
        kdeGrid <- reshape::melt(kde[[b]]$spatial.z)[c(1:3)]
        colnames(kdeGrid) <- c("longitude","latitude","kde")
        kdeGrid$dataType <- "estimated"
      }

      if(b == "all"){print("Not possible to plot true state for b = 'all'")




        if(dataType == "sim"){
         # kdeGridTrue <- numeric(4)
        #  gridTmp <- expand.grid(longitude = seq(xlim[1],xlim[2],length.out = res),
        #                         latitude = seq(ylim[1],ylim[2],length.out = res))
        #  for(t in 1:T){
        #    tmp <- gridTmp
        #    tmp$kde <- apply(gridTmp,1,function(x){f_f(x,t = t, b = b,markRecaptureObject,p = p)})
        #    tmp$time <- t
        #    kdeGridTrue <- rbind(kdeGridTrue,tmp)
        #  }
        #  kdeGridTrue <- kdeGridTrue[-1,]

          #kdeGridTrue$dataType <- "true"
          #kdeGrid <- as.data.frame(rbind(kdeGrid,kdeGridTrue))

          #if(log){
          #  trans <- "log"
          #  my_breaks <- exp(seq(min(log(kdeGrid$kde[kdeGrid$kde!=0]),na.rm = TRUE),
          #                       max(kdeGrid$kde,na.rm = TRUE),length.out = 7))[2:6]
          #
          #}else{
          #  trans <- "identity"
          #  my_breaks <- seq(min(kdeGrid$kde,na.rm = TRUE),
          #                   max(kdeGrid$kde,na.rm = TRUE),length.out = 7)[2:6]
          #}


        }



        }else{

          if(dataType == "sim"){
            kdeGridTrue <- numeric(4)
            gridTmp <- expand.grid(longitude = seq(xlim[1],xlim[2],length.out = res),
                             latitude = seq(ylim[1],ylim[2],length.out = res))
            for(t in 1:T){
              tmp <- gridTmp
              tmp$kde <- apply(gridTmp,1,function(x){f_f(x,t = t, b = b,markRecaptureObject,p = p)})
              tmp$time <- t
              kdeGridTrue <- rbind(kdeGridTrue,tmp)
            }
          kdeGridTrue <- kdeGridTrue[-1,]

          kdeGridTrue$dataType <- "true"
          kdeGrid <- as.data.frame(rbind(kdeGrid,kdeGridTrue))
          }


        }

        if(log){
          trans <- "log"
          my_breaks <- exp(seq(min(log(kdeGrid$kde[kdeGrid$kde!=0]),na.rm = TRUE),
                             max(kdeGrid$kde,na.rm = TRUE),length.out = 7))[2:6]
        }else{
          trans <- "identity"
          my_breaks <- seq(min(kdeGrid$kde,na.rm = TRUE),
                         max(kdeGrid$kde,na.rm = TRUE),length.out = 7)[2:6]
        }

      pg <- ggplot2::ggplot()+
        ggplot2::geom_tile(data = kdeGrid, ggplot2::aes(longitude, latitude,fill = kde)) +
        #ggplot2::geom_contour(data = kdeGrid, ggplot2::aes(longitude, latitude, z =kde,colour = ..level..))+

        ggplot2::ggtitle(paste("breeding area",b))+
        ggplot2::scale_fill_distiller("kde", palette = "Spectral",
                                      trans = trans,# limits = c(4e-44,1),
                                      breaks = my_breaks, labels = formatC(my_breaks,format="e",digits=1))+
        ggplot2::theme(text = ggplot2::element_text(size = 23))

      if(drawBoundaries){
        pg <- pg +
          ggplot2::geom_sf(data = countryBoundaries, color = "grey30",fill = "white", size = .3) +
          ggplot2::coord_sf(xlim = xlim,
                            ylim = ylim,
                            expand = FALSE)
      }

      if(facetByTime){
        pg <- pg + ggplot2::facet_wrap(~time)
      }
      if(dataType == "sim"){
        pg <- pg + ggplot2::facet_grid(~dataType)
      }
      if(pdf) plot(pg)

    }
  if(pdf) dev.off()
  if(dim == 2) pg
}
