#' plot age distribution of raw recoveries of real world data
#'
#' This function plots the age distribution of recoveries in the real world data as a histogram.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param pdfName string to name pdf-file, please include ".pdf"!
#' @return histogram of age of raw recoveries, depending on arguments plot as pdf or to plot to device
#' @export
#' @examples plotAgeDistribution()

plotAgeDistribution <- function(markRecaptureObject, pdf = FALSE, pdfName = "ageDistribution.pdf",
                              areaNames = NULL, freq = TRUE){
  if(pdf) pdf(pdfName)
    age <- do.call("rbind",markRecaptureObject$winteringArea$data)$age
    T <- markRecaptureObject$observationTime
    hist(age, breaks = 0:T + 0.5, freq = freq)
  if(pdf) dev.off()
}
