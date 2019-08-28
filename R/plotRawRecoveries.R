#' plot raw recoveries of real world data
#'
#' This function plots the distribution of recoveries in the real world data in Europe and Africa.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param pdfName string to name pdf-file, please include ".pdf"!
#' @param areaNames optional, order of breeding area names to be plotted, defaults to NULL.
#' In the default case order of breeding areas in markRecaptureObject will be chosen.
#' @return depending on arguments plot as pdf or to plot to device
#' @export
#' @examples plotRawRecoveries()

plotRawRecoveries <- function(markRecaptureObject, pdf = FALSE, pdfName = "rawRecoveries.pdf",
                              areaNames = NULL,facetByAge = FALSE,
                              ageMin = 0, ageMax = NULL){
  if(pdf) pdf(pdfName)

  if(is.null(areaNames)) areaNames <- names(markRecaptureObject$breedingAreas)
  myMap<-ggmap::get_stamenmap(bbox=c(left=-24, bottom = -35, right = 71, top = 71),
                              zoom = 3,maptype = "terrain-background")

  dat <- do.call("rbind", markRecaptureObject$winteringArea$data)

  if(is.null(ageMax)){ageMax <- max(dat$age)}

  dat <- dat[dat$age > ageMin & dat$age <= ageMax & dat$markArea %in% areaNames,]


  pl <- ggmap::ggmap(myMap)+
    ggplot2::geom_point(data=dat,ggplot2::aes(x=recLon,y=recLat),size = 0.5)
  if(facetByAge){pl <- pl + ggplot2::facet_grid(markArea~age)}
  plot(pl)

  if(pdf) dev.off()
}
