#' plot age distribution of raw recoveries of real world data
#'
#' This function plots the age distribution of recoveries in the real world data
#' as a histogram.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param pdfName string to name pdf-file, please include ".pdf"!
#' @return histogram of age of raw recoveries, depending on arguments plot as
#' pdf or to plot to device
#' @export
#' @examples plotAgeDistribution(mro2D)
plotAgeDistribution <- function(markRecaptureObject, pdf = FALSE,
                                pdfName = "ageDistribution.pdf") {
  if (pdf) pdf(pdfName)
  age <- as.data.frame(
    do.call("rbind", markRecaptureObject$winteringArea$recoveryData)$age
  )
  colnames(age) <- "age"
  pl <- ggplot2::ggplot(data = age, ggplot2::aes(x = age)) +
    ggplot2::geom_histogram(
      binwidth = 1, boundary = 0.4,
      fill = "black", col = "grey"
    ) +
    ggplot2::labs(x = "age", y = "absolute frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 24))
  plot(pl)
  if (pdf) {
    grDevices::dev.off()
  }
  return(pl)
}
