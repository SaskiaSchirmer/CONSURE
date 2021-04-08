#' plot raw recoveries of real world data
#'
#' This function plots the distribution of recoveries in the real world data in Europe and Africa.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param pdfName string to name pdf-file, please include ".pdf"!
#' @param areaNames optional, order of breeding area names to be plotted, defaults to NULL.
#' In the default case order of breeding areas in markRecaptureObject will be chosen.
#' @param facetByAge logical, determines if the data should be facetted by the age column.
#' Defaults to FALSE.
#' @param facetByArea logical, determines if the data should be facetted by the area column.
#' Defaults to FALSE.
#' @param ageMin numeric. Defaults to 0. If set only data points older than ageMin are plotted.
#' @param ageMax numeric. If set only data points younger or equal ageMax are plotted. Defaults to NULL.
#' @param left numeric. Western border of map. Defaults to -24.
#' @param bottom numeric. Southern border of map. Defaults to -35.
#' @param right numeric. Eastern border of map. Defaults to 71.
#' @param top numeric. Northern border of map. Defaults to 71.
#' @return depending on arguments plot as pdf or to plot to device
#' @export
#' @examples plotRawRecoveries()

plotRawRecoveries <- function(markRecaptureObject, pdf = FALSE, pdfName = "rawRecoveries.pdf",
                              areaNames = NULL,facetByAge = FALSE,facetByArea  = FALSE,
                              ageMin = 0, ageMax = NULL,
                              left = -24, bottom = -35, right = 71, top = 71,
                              xname = "longitude", yname = "latitude",
                              timename = "age", markAreaName = "markArea",
                              plotTitle = paste(areaNames, sep = "")){

  dim <- markRecaptureObject$spatialDim

  if(pdf) pdf(pdfName)

  if(dim == 1){
    dat <- do.call("rbind", markRecaptureObject$winteringArea$recoveryData)

    pl <- ggplot2::ggplot(ggplot2::aes_string(xname,timename),
                    data = dat) +
      ggplot2::labs(x = "longitude", y = "age",title = plotTitle)+
      ggplot2::geom_point(shape = 3)

    if(facetByArea){pl <- pl + ggplot2::facet_grid(reformulate(".",markAreaName))}
    if(!facetByAge) message("Not facetting by age creates no meaningful plot. Facetting by age anyways.")
  } else if(dim == 2){

    if(is.null(areaNames)) areaNames <- names(markRecaptureObject$breedingAreas)
    myMap<-ggmap::get_stamenmap(bbox=c(left=left, bottom = bottom, right = right, top = top),
                              zoom = 3,maptype = "terrain-background")

    dat <- do.call("rbind", markRecaptureObject$winteringArea$recoveryData)

    if(is.null(ageMax)){ageMax <- max(dat[timename])}

    dat <- dat[dat[timename] > ageMin & dat[timename] <= ageMax & as.character(unlist(dat[markAreaName])) %in% areaNames,]


    pl <- ggmap::ggmap(myMap)+
      ggplot2::geom_point(data=dat,ggplot2::aes_string(x=xname,y=yname#,col = as.factor(winter)),
                                              ),size = 2)+#, alpha = .5)+
      ggplot2::labs(x = "longitude", y = "latitude",title = plotTitle)+
      ggplot2::theme(text = ggplot2::element_text(size = 24))
    if(facetByAge){
      if(facetByArea){pl <- pl + ggplot2::facet_grid(reformulate(timename,markAreaName))}else{
        pl <- pl + ggplot2::facet_grid(reformulate(timename,"."))}
      }else if(facetByArea){pl <- pl + ggplot2::facet_grid(reformulate(".",markAreaName))}
  }

  if(pdf){plot(pl)
    dev.off()}
  pl
}
