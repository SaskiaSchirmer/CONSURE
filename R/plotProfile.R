#' Defaults to TRUE.
# CONSURE - Continuous Survival, Use of Space and Recovery Probability Estimates.
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

#' plot parameter values (and bootstrape confidence interval, if
#' available) along a profile line
#'
#' This function plots the parameter values along a profile line given in the
#' sfc-object profileOfParameter.
#' @param profileOfParameter profile of a raster object along a segmented
#' profile line.
#' @param ylab name of the y-axis, which is normally the name of the parameter
#'
#' @return vector of length res with survival probabilities dependent on space
#' @export
#' @examples plotProfile(profileOfParameter, ylab = "survival")

plotProfile <- function(profileOfParameter, ylab){

  profile <- ggplot2::ggplot()

  if("bootstrapLowerQuantile" %in% names(profileOfParameter) &
     "bootstrapUpperQuantile" %in% names(profileOfParameter)){

    profile <- profile +
      ggplot2::geom_ribbon(data = profileOfParameter,
                           ggplot2::aes(x = units::drop_units(dist),
                                        ymin = bootstrapLowerQuantile,
                                        ymax = bootstrapUpperQuantile,
                                        color = "variability"), alpha = 0.7,
                           fill = "grey")
  }

  range <- range(units::drop_units(profileOfParameter$dist))

  profile <- profile +
    ggplot2::geom_point(data = profileOfParameter,
                        ggplot2::aes(units::drop_units(dist),
                                     parameterValue, color = "estimate"))+
    ggplot2::labs(x = "distance [km]", y = ylab) +
    ggplot2::labs(color  = "type") +
    ggplot2::scale_x_continuous(breaks = seq(range[1],range[2],
                                             by = 1000000),
                                labels = seq(range[1],range[2],
                                             by = 1000000)/1000,
                                limits =)+
    ggplot2::scale_colour_manual("",
                                 breaks = c("variability", "estimate"),
                                 values = c("grey", "black"),
                                 labels = c("bootstrap\nconfidence\ninterval", "estimate")) +
    ggplot2::ylim(c(0,1)) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 20),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 10,
                                                                   unit = "pt")),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
      axis.line = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "transparent"),
      panel.spacing.x = ggplot2::unit(30, "mm"),
      panel.spacing.y = ggplot2::unit(10, "mm"),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )

  profile
}
