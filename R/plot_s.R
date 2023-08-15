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

#' plot true and estimated survival function
#'
#' This function estimates the survival from a kernel density estimate of the
#' data of recovered individuals. It uses the data of all areas of origin at
#' once.
#' @param mark_recapture_object object of class mark_recapture_object
#' (see mark_recapture_object())
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param true_values_available logical, use TRUE for simulated data, FALSE for
#' real-world data. Defaults to FALSE.
#' @param draw_boundaries logical, country boundaries will be drawn, if TRUE.
#' Only works if coordinates are in longitude/latitude system. Defaults to
#' FALSE.
#' @param xlb if not NULL, it zooms the plot to the limits given by xlim and
#' ylim
#' @param zlim boundaries in the direction of survival values
#' @param lon vector of longitude. Defaults to NULL.
#' @param lat vector of latitude. Defaults to NULL.
#' @param no_ci suppresses drawing the confidence interval, even if bootstrap
#' information is in the mark_recapture_object. Defaults to FALSE.
#' @param profile_of_parameter sf-object containing the information of the
#' profile line, along which the values of a parameter can be plotted including
#' the bootstrap confidence interval. If this information is given, the profile
#'  line will be plotted
#'
#' @importFrom rlang .data
#' @return vector of length res with survival probabilities dependent on space
#' @export
#' @examples plot_s(mro1D, true_values_available = TRUE)
plot_s <- function(mark_recapture_object, pdf = FALSE,
                   true_values_available = FALSE,
                   draw_boundaries = FALSE, xlb = NULL,
                   zlim = c(0, 1), lon = NULL, lat = NULL,
                   no_ci = FALSE, profile_of_parameter = NULL) {
  res <- mark_recapture_object$spatial_resolution
  s <- mark_recapture_object$destination$survival
  s_fit <- mark_recapture_object$estimates$s
  dim <- mark_recapture_object$spatial_dimension
  xlim <- mark_recapture_object$destination$window$xrange
  ylim <- mark_recapture_object$destination$window$yrange
  bootstrap_quants <-
    mark_recapture_object$estimates$bootstrap$bootstrap_quantiles
  bootstrap <- bootstrap_quants[bootstrap_quants$parameter == "s", ]
  crs <- mark_recapture_object$destination$crs

  if (is.null(lon)) lon <- mark_recapture_object$kde$all$z$`1`$xcol
  if (is.null(lat)) lat <- mark_recapture_object$kde$all$z$`1`$yrow

  if (pdf) {
    pdf(
      paste("estimate_s_", format(Sys.time(), "%H%M%S_%d%m%Y"), ".pdf",
        sep = ""
      ),
      width = 9, height = 6
    )
  }

  if (dim == 1) {
    dat <- data.frame(
      x = seq(xlim[1], xlim[2], length.out = res), y = c(s_fit),
      data_type = "estimated"
    )

    dat2 <- NULL
    if (true_values_available) {
      dat2 <- data.frame(x = seq(xlim[1], xlim[2], length.out = res))
      dat2$y <- s(seq(xlim[1], xlim[2], length.out = res))
      dat2$data_type <- "true"
    }
    dat <- rbind(dat, dat2)
    plot_s <- ggplot2::ggplot()

    if (!is.null(bootstrap) && !no_ci) {
      plot_s <- plot_s +
        ggplot2::geom_ribbon(
          data = bootstrap, ggplot2::aes(
            x = lon,
            ymin = .data$lq,
            ymax = .data$uq,
            linetype = "variability",
            color = "variability"
          ),
          alpha = 0.7, fill = "grey"
        )
    }

    plot_s <- plot_s +
      ggplot2::geom_line(
        ggplot2::aes(
          x = .data$x, y = .data$y,
          linetype = .data$data_type, color = .data$data_type
        ),
        data = dat, size = 1.5
      ) +
      ggplot2::labs(
        x = "destination area", y = "survival"
      ) +
      ggplot2::theme(text = ggplot2::element_text(size = 20)) +
      ggplot2::coord_cartesian(ylim = zlim)

    if (!is.null(bootstrap) && !no_ci) {
      plot_s <- plot_s +
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
      plot_s <- plot_s +
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
    s_grid <- reshape::melt(s_fit)
    s_grid$X1 <- rep(lon, each = res)
    s_grid$X2 <- rep(lat, res)
    s_grid$data_type <- "estimated"
    colnames(s_grid) <- c("longitude", "latitude", "s", "data_type")

    if (true_values_available) {
      s_grid_true <- expand.grid(
        longitude = lon,
        latitude = lat
      )
      attr(s_grid_true, "out.attrs") <- NULL
      s_grid_true$s <- apply(s_grid_true, 1, s)
      s_grid_true$data_type <- "true"
      s_grid <- as.data.frame(rbind(s_grid, s_grid_true))
    }

    plot_s <- ggplot2::ggplot() +
      ggplot2::geom_tile(data = s_grid, ggplot2::aes(.data$longitude,
        .data$latitude,
        fill = .data$s
      )) +
      ggplot2::labs(fill = "estimated\n survival") +
      ggplot2::scale_fill_viridis_c("survival",
        limits = zlim,
        na.value = "grey90"
      ) +
      ggplot2::theme(text = ggplot2::element_text(size = 20))

    if (draw_boundaries) {
      plot_s <- plot_s +
        ggplot2::borders("world", colour = "grey30", size = 1) +
        ggplot2::coord_sf(
          xlim = xlim,
          ylim = ylim,
          expand = FALSE
        )
    }

    if (true_values_available) {
      plot_s <- plot_s +
        ggplot2::facet_grid(~data_type) +
        ggplot2::labs(fill = "estimated\n survival") +
        ggplot2::scale_fill_viridis_c("survival",
          limits = zlim,
          na.value = "grey90"
        ) +
        ggplot2::theme(text = ggplot2::element_text(size = 20))
    }

    if (!is.null(profile_of_parameter)) {
      plot_s <- plot_s +
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

    plot_s <- plot_s +
      ggplot2::coord_sf(
        expand = FALSE,
        crs = sf::st_crs(crs)
      )
  }
  if (pdf) {
    plot(plot_s)
    grDevices::dev.off()
  }

  plot_s
}
