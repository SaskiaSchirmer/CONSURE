#' plot kernel density estimate and true density for simulated data
#'
#' This function plots the kernel density estimate and true density for simulated data.
#' @inheritParams p_nf
#' @param res resolution for longitude and latitude
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param ylim vector in the form of c(ymin,ymax): limits of the y-axis. Defaults to c(0,3).
#' @param trueValuesAvailable logical, use TRUE for simulated data, FALSE for real-world data. Defaults to FALSE.
#' @param log logical, uses log-scale for kernel-density-values if TRUE. Defaults to FALSE.
#' @param facetByTime logical, plots one plot for every age class if TRUE. Defaults to TRUE.
#' @param drawBoundaries logical, country boundaries will be drawn, if TRUE. Defaults to TRUE.
#' @return depending on arguments plot as pdf or to plot device
#' @export
#' @examples plotKDE()

# plot kernel density estimate and true density
plotKDE <- function(b,markRecaptureObject, pdf = FALSE, ylim = c(0,1.5),trueValuesAvailable=FALSE,
                    log=FALSE, facetByTime = TRUE,drawBoundaries = TRUE){

  res <- markRecaptureObject$spatialResolution
  T <- markRecaptureObject$observationTime
  xlim <- markRecaptureObject$winteringArea$window$xrange
  kde <- markRecaptureObject$kde
  dim <- markRecaptureObject$spatialDim
  if(trueValuesAvailable){if(b=="all"&dim==2){}else{p <- 1-p_nf(b,markRecaptureObject)}}


  if(pdf) pdf("KDE.pdf")
    if(dim == 1){
      tmp <- reshape::melt(kde[[b]]$z)
      tmp <- dplyr::group_by(tmp, value.x,L1)
      tmp$L1 <- as.factor(as.numeric(tmp$L1))
      tmp <- dplyr::summarise(tmp, y = mean(value.value))
      colnames(tmp) <- c("x","age","y")
      tmp$dataType <- "estimated"

      if(trueValuesAvailable){
        tmp2 <- expand.grid(x = seq(xlim[1],xlim[2],length.out = res),age = 1:T)
        tmp2$y <- apply(tmp2,1,function(x) f_f(x["x"],x["age"],b, markRecaptureObject,p))
        tmp2$age <- as.factor(tmp2$age)
        tmp2$dataType <- "true"

        tmp <- rbind(tmp,tmp2)
      }

      pg <- ggplot2::ggplot() +
        ggplot2::geom_line(ggplot2::aes(x = x,
                                        y = y, col = age, linetype = dataType),
                           data = tmp, size = 1.5)+
        ggplot2::scale_color_viridis_d(end = 0.9)+
        ggplot2::labs(x = "non-breeding area", y = "density",
                      linetype = "datatype", color = "age",title = b)+
        ggplot2::theme(text = ggplot2::element_text(size = 20))
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




        if(trueValuesAvailable){
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

          if(trueValuesAvailable){
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

          if(!facetByTime){
            tmp <- tidyr::spread(kdeGridTrue, time, kde)
            kdeGridTrue <- tmp[,1:2]
            kdeGridTrue$kde <- rowSums(tmp[,3:(T+2)], na.rm = TRUE)
          }

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
      #  ggplot2::geom_tile(data = kdeGrid, ggplot2::aes(longitude, latitude,fill = kde)) +
        #ggplot2::geom_contour(data = kdeGrid, ggplot2::aes(longitude, latitude, z =kde,colour = ..level..))+

        ggplot2::ggtitle(paste(b))+
        ggplot2::scale_fill_viridis_c("kde",
                                      trans = trans,# limits = c(4e-44,1),
                                      breaks = my_breaks, labels = formatC(my_breaks,format="e",digits=1))+
        ggplot2::theme(text = ggplot2::element_text(size = 23))

      if(!trueValuesAvailable){
        pg <- pg + ggplot2::geom_tile(data = kdeGrid, ggplot2::aes(longitude, latitude,fill = kde))
      }else{
        pg <- pg + ggplot2::geom_tile(data = kdeGrid, ggplot2::aes(longitude, latitude,fill = kde),
                                            height = 1/res,width = 1/res)
      }

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
      if(trueValuesAvailable){
        pg <- pg + ggplot2::facet_grid(~dataType)
      }
    }
  if(pdf){if(pdf) plot(pg); dev.off()}
  pg
}
