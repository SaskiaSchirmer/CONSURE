# CONSURE - Continuous Survival, Use of Space and Recovery Probability
# Estimates.
# Copyright (C) 2021  Saskia Schirmer
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
