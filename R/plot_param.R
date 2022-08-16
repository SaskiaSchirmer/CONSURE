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

#' plot true and estimated migratory connectivity
#'
#' This function plots the kernel density estimate and true density for
#' simulated data.
#' @param mark_recapture_object object of class mark_recapture_object
#' (see mark_recapture_object())
#' @param b specifies the area of origin for which the plot is drawn. Can be
#' either a origin_name, the corresponding number of the areas of origin or
#' "all" for all areas of origin at once.
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param log plots logarithm of migratory connectivity. Defaults to FALSE.
#' @param true_values_available logical, use TRUE for simulated data, FALSE for
#' real-world data. Defaults to FALSE.
#' @param uq upper quantile until which migratory connectivity value is plotted
#' @param draw_boundaries logical, country boundaries will be drawn, if TRUE.
#' Only works if coordinates are in longitude/latitude system. Defaults to
#' FALSE.
#' @param no_ci suppresses drawing the confidence interval, even if bootstrap
#' information is in the mark_recapture_object. Defaults to FALSE.
#' @param profile_of_parameter sf-object containing the information of the
#' profile line, along which the values of a parameter can be plotted including
#' the bootstrap confidence interval. If this information is given, the profile
#' line will be plotted
#'
#' @importFrom rlang .data
#' @return depending on arguments plot as pdf or to plot device
#' @export
#' @examples plot_m(mro1D, true_values_available = TRUE)
plot_param <- function(mark_recapture_object, b = "all",
                   param, pdf = FALSE, log = FALSE,
                   true_values_available = FALSE, uq = 1,
                   draw_boundaries = FALSE,
                   no_ci = FALSE, profile_of_parameter = NULL) {
  if (pdf) {
    pdf(paste("plot_",param,"_",
              format(Sys.time(), "%H%M%S_%d%m%Y"), ".pdf", sep = ""),
        width = 17, height = 10
    )
  }
  xlim <- mark_recapture_object$destination$window$xrange
  param_fit <- mark_recapture_object$estimates[[param]]
  bootstrap_quants <-
    mark_recapture_object$estimates$bootstrap$bootstrap_quantiles
  bootstrap <- bootstrap_quants[bootstrap_quants$parameter == param &
                                  bootstrap_quants$mark_area == b, ]

  dim <- mark_recapture_object$spatial_dimension
  res <- mark_recapture_object$spatial_resolution
  origin_names <- names(mark_recapture_object$origins)[
    names(mark_recapture_object$origins) != "all"
  ]
  lon <- mark_recapture_object$kde$all$z$`1`$xcol
  lat <- mark_recapture_object$kde$all$z$`1`$yrow
  crs <- mark_recapture_object$destination$crs

  if (dim == 1) {
    dat <- data.frame(x = seq(xlim[1], xlim[2], length.out = res))

    if(param == "m") {
      dat$y <- c(param_fit[[b]])
    } else {
      dat$y <- c(param_fit)
    }

    dat$data_type <- "estimated"

    dat2 <- NULL
    if (true_values_available) {
      dat2 <- data.frame(x = seq(xlim[1], xlim[2], length.out = res))

      if(param == "m") {
        dat2$y <-
          mark_recapture_object$origins[[b]]$migratory_connectivity(dat2$x)
      } else if(param == "s") {
        dat2$y <-
          mark_recapture_object$destination$survival(dat2$x)
      }

      dat2$data_type <- "true"
    }

    dat <- rbind(dat, dat2)

    plot_m <- ggplot2::ggplot()

    if (!no_ci && !is.null(bootstrap)) {
      plot_m <- plot_m + ggplot2::geom_ribbon(
        data = bootstrap,
        ggplot2::aes(
          x = lon, ymin = .data$lq, ymax = .data$uq,
          linetype = "variability", color = "variability"
        ),
        alpha = 0.7, fill = "grey"
      )
    }

    plot_m <- plot_m +
      ggplot2::geom_line(ggplot2::aes(
        x = .data$x, y = .data$y,
        linetype = .data$data_type, color = .data$data_type
      ),
      data = dat, size = 1.5
      ) +
      ggplot2::labs(
        x = "destination area", y = "migratory connectivity",
        linetype = "data_type"
      ) +
      ggplot2::theme(text = ggplot2::element_text(size = 20))

    if (!no_ci && !is.null(bootstrap)) {
      plot_m <- plot_m +
        ggplot2::labs(
          color = "Guide name", linetype = "Guide name",
          shape = "Guide name"
        ) +
        ggplot2::scale_colour_manual("",
                                     breaks = c("variability", "estimated", "true"),
                                     values = c("grey", "black", "black")
        ) +
        ggplot2::scale_linetype_manual("",
                                       breaks = c("variability", "estimated", "true"),
                                       values = c(1, 1, 2)
        )
    } else {
      plot_m <- plot_m +
        ggplot2::labs(
          color = "Guide name", linetype = "Guide name",
          shape = "Guide name"
        ) +
        ggplot2::scale_colour_manual("",
                                     breaks = c("estimated", "true"),
                                     values = c("black", "black")
        ) +
        ggplot2::scale_linetype_manual("",
                                       breaks = c("estimated", "true"),
                                       values = c(1, 2)
        )
    }
  } else if (dim == 2) {
    ylim <- mark_recapture_object$destination$window$yrange

    m_grid <- reshape2::melt(param_fit)
    m_grid$Var1 <- rep(lon, each = res)
    m_grid$Var2 <- rep(lat, res)
    colnames(m_grid) <- c("longitude", "latitude", "m", "origin")
    m_grid <- m_grid[m_grid$origin == b, ]
    m_grid$data_type <- "estimated"

    if (true_values_available) {
      if(param == "m") {
        m <- mark_recapture_object$origins[[b]]$migratory_connectivity
      } else if(param == "s") {
        m <- mark_recapture_object$destination$survival
      }

      m_grid_true <- expand.grid(
        longitude = lon,
        latitude = lat,
        origin = origin_names
      )
      m_grid_true$m <- apply(m_grid_true, 1, function(x) {
        m(as.numeric(x[1:2]))
      })

      m_grid_true$data_type <- "true"
      m_grid <- as.data.frame(rbind(m_grid, m_grid_true))
    }

    plot_m <- ggplot2::ggplot() +
      ggplot2::labs(fill = "estimated\n migratory\n connectivity") +
      ggplot2::scale_fill_viridis_c("connectivity", na.value = "grey90") +
      ggplot2::theme(text = ggplot2::element_text(size = 20))

    if (b != "all") {
      plot_m <- plot_m + ggplot2::facet_grid(~origin)
    }

    if (true_values_available) {
      plot_m <- plot_m +
        ggplot2::facet_grid(data_type ~ .)
    }

    plot_m <- plot_m + ggplot2::geom_tile(
      data = m_grid,
      ggplot2::aes(.data$longitude,
                   .data$latitude,
                   fill = .data$m
      )
    )

    if (draw_boundaries) {
      plot_m <- plot_m +
        ggplot2::borders("world", colour = "grey30", size = 1) +
        ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE)
    }

    if (!is.null(profile_of_parameter)) {
      plot_m <- plot_m +
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

    plot_m <- plot_m +
      ggplot2::coord_sf(
        expand = FALSE,
        crs = crs
      )
  }

  if (pdf) {
    plot(plot_m)
    grDevices::dev.off()
  }

  return(plot_m)
}
