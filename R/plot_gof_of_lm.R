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

#' plot goodness of fit for linear model used to estimate parameter functions
#'
#' This function plots the R^2 values of the robust or ordinary linear
#' regression used to estimate survival.
#' @param mark_recapture_object object of class mark_recapture_object
#' (see mark_recapture_object())
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param xlb numeric, lower bound of x. Defaults to NULL.
#' @param xub numeric, upper bound of x. Defaults to NULL.
#' @param ylb numeric, lower bound of y. Defaults to NULL.
#' @param yub numeric, upper bound of y. Defaults to NULL.
#' @param draw_boundaries logical, country boundaries will be drawn, if TRUE.
#' Defaults to TRUE.
#' @param profile_of_parameter sf-object containing the information of the
#' profile line, along which the values of a parameter can be plotted including
#' the bootstrap confidence interval. If this information is given, the profile
#'  line will be plotted
#'
#' @return matrix of dimension res*res with R^2-values for the linear model in
#' every point.
#' @export
#' @examples plot_gof_of_lm(mro1D)
plot_gof_of_lm <- function(mark_recapture_object, pdf = FALSE,
                           xlb = NULL, xub = NULL, ylb = NULL, yub = NULL,
                           draw_boundaries = TRUE, no_ci = FALSE,
                           profile_of_parameter = NULL) {
  gof <- mark_recapture_object$estimates$lm$all$gof
  dim <- mark_recapture_object$spatial_dimension
  xlim <- mark_recapture_object$destination$window$xrange
  ylim <- mark_recapture_object$destination$window$yrange
  lon <- mark_recapture_object$kde$all$z$`1`$xcol
  lat <- mark_recapture_object$kde$all$z$`1`$yrow
  res <- mark_recapture_object$spatial_resolution
  bootstrap_quants <-
    mark_recapture_object$estimates$bootstrap$bootstrap_quantiles
  bootstrap <- bootstrap_quants[bootstrap_quants$parameter == "gof", ]
  crs <- mark_recapture_object$destination$crs

  if (pdf) pdf("GOFofS.pdf", width = 9, height = 6)

  if (dim == 1) {
    plot_gof <- ggplot2::ggplot()

    if (!no_ci && !is.null(bootstrap)) {
      plot_gof <- plot_gof +
        ggplot2::geom_ribbon(
          data = bootstrap,
          ggplot2::aes(
            x = lon, ymin = .data$lq,
            ymax = .data$uq,
            color = "variability",
            linetype = "variability"
          ),
          alpha = 0.7, fill = "grey"
        )
    }

    plot_gof <- plot_gof +
      ggplot2::geom_line(ggplot2::aes(
        x = seq(xlim[1], xlim[2], length.out = res),
        y = gof, color = "estimate", linetype = "estimate"
      ), size = 1.5) +
      ggplot2::labs(x = "destination area", y = expression(R^2)) +
      ggplot2::theme(text = ggplot2::element_text(size = 20))

    if (!no_ci && !is.null(bootstrap)) {
      plot_gof <- plot_gof +
        ggplot2::scale_colour_manual("",
          breaks = c("variability", "estimate"),
          values = c("grey", "black")
        ) +
        ggplot2::scale_linetype_manual("",
          breaks = c("variability", "estimate"),
          values = c(1, 1)
        ) +
        ggplot2::scale_x_continuous(breaks = c(0, 0.5, 1)) +
        ggplot2::labs(
          color = "Guide name", linetype = "Guide name",
          shape = "Guide name"
        )
    } else {
      plot_gof <- plot_gof +
        ggplot2::scale_colour_manual("",
          breaks = c("estimate"),
          values = c("black")
        ) +
        ggplot2::scale_linetype_manual("",
          breaks = c("estimate"),
          values = c(1)
        ) +
        ggplot2::scale_x_continuous(breaks = c(0, 0.5, 1)) +
        ggplot2::labs(
          color = "Guide name", linetype = "Guide name",
          shape = "Guide name"
        )
    }
  } else if (dim == 2) {
    gof_grid <- reshape::melt(gof)
    gof_grid$X1 <- rep(lon, each = res)
    gof_grid$X2 <- rep(lat)
    gof_grid$data_type <- "estimated"
    colnames(gof_grid) <- c("longitude", "latitude", "gof", "data_type")

    plot_gof <- ggplot2::ggplot() +
      ggplot2::geom_tile(data = gof_grid, ggplot2::aes(.data$longitude,
        .data$latitude,
        fill = .data$gof
      )) +
      ggplot2::labs(fill = "estimated\n survival") +
      ggplot2::scale_fill_viridis_c(expression(R^2),
        limits = c(0, 1)
      ) +
      ggplot2::theme(text = ggplot2::element_text(size = 20))
    if (draw_boundaries) {
      plot_gof <- plot_gof +
        ggplot2::borders("world", colour = "grey30", size = 1) +
        ggplot2::coord_sf(
          xlim = xlim,
          ylim = ylim,
          expand = FALSE
        )
    }
    if (!is.null(xlb)) {
      plot_gof <- plot_gof +
        ggplot2::coord_cartesian(xlim = c(xlb, xub), ylim = c(ylb, yub))
    }

    if (!is.null(profile_of_parameter)) {
      plot_gof <- plot_gof +
        ggplot2::geom_line(
          data = data.frame(
            x_df = unname(sf::st_coordinates(profile_of_parameter)[
              c(1, nrow(profile_of_parameter)), 1
            ]),
            y_df = unname(sf::st_coordinates(profile_of_parameter)[
              c(1, nrow(profile_of_parameter)), 2
            ]),
            data_type = "estimated"
          ),
          ggplot2::aes(
            x = .data$x_df,
            y = .data$y_df
          ), size = 2
        )
    }

    plot_gof <- plot_gof +
      ggplot2::coord_sf(
        expand = FALSE,
        crs = sp::CRS(crs)
      )
  }
  if (pdf) plot(plot_gof)
  if (pdf) grDevices::dev.off()
  plot_gof
}
