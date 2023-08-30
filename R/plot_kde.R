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

#' plot kernel density estimate and true density for simulated data
#'
#' This function plots the kernel density estimate and true density for
#' simulated data.
#' @inheritParams p_nf
#' @param pdf logical, saves image as pdf-file if TRUE. Defaults to FALSE.
#' @param ylim vector in the form of c(ymin,ymax): limits of the y-axis.
#' Defaults to c(0,3).
#' @param true_values_available logical, use TRUE for simulated data, FALSE for
#' real-world data. Defaults to FALSE.
#' @param log logical, uses log-scale for kernel-density-values if TRUE.
#' Defaults to FALSE.
#' @param age_min numeric. Defaults to 0. If set only data points older than
#' age_min are plotted.
#' @param age_max numeric. If set only data points younger or equal age_max are
#' plotted. Defaults to NULL.
#' @param draw_boundaries logical, country boundaries will be drawn, if TRUE.
#' Defaults to TRUE.
#' @param title logical, if TRUE the name of the area of origin will be
#' added as the title. If FALSE, no title will be added. Defaults to TRUE.
#' @return depending on arguments plot as pdf or to plot device
#' @importFrom rlang .data
#' @export
#' @examples plot_kde("all", mro1D, true_values_available = TRUE)
plot_kde <- function(b, mark_recapture_object, pdf = FALSE, ylim = c(0, 1.5),
                     true_values_available = FALSE,
                     log = FALSE,
                     age_min = 0, age_max = NULL,
                     draw_boundaries = FALSE,
                     title = TRUE) {
  main <- ""
  if (title) main <- b
  res <- mark_recapture_object$spatial_resolution
  o_t <- mark_recapture_object$observation_time
  dim <- mark_recapture_object$spatial_dimension
  win <- mark_recapture_object$destination$window
  xlim <- win$xrange
  kde <- mark_recapture_object$kde
  crs <- mark_recapture_object$destination$crs

  longitude <- mark_recapture_object$kde[[b]]$z$`1`$xcol
  latitude <- mark_recapture_object$kde[[b]]$z$`1`$yrow

  if (dim == 1) {
    normalize <- mark_recapture_object$destination$window$xrange[2] -
      mark_recapture_object$destination$window$xrange[1]
  } else if (dim == 2) {
    normalize <- spatstat.geom::area(win)
  }

  if (true_values_available) {
    if (b == "all" && dim == 2) {
    } else {
      p <- (normalize - p_nf(b, mark_recapture_object)) / normalize
    }
  }


  if (pdf) pdf("KDE.pdf")
  if (dim == 1) {
    tmp <- reshape::melt(kde[[b]]$z)
    tmp <- dplyr::group_by(tmp, .data$value.x, .data$L1)
    tmp$L1 <- as.numeric(tmp$L1)
    tmp <- dplyr::summarise(tmp, y = mean(.data$value.value))
    colnames(tmp) <- c("x", "age", "y")
    tmp$data_type <- "estimated"

    if (true_values_available) {
      tmp2 <- expand.grid(
        x = seq(xlim[1], xlim[2], length.out = res),
        age = 1:o_t
      )
      tmp2$y <- apply(
        tmp2, 1,
        function(x) {
          f_f(
            x["x"], x["age"], b, mark_recapture_object,
            p
          )
        }
      )
      tmp2$age <- tmp2$age
      tmp2$data_type <- "true"

      tmp <- rbind(tmp, tmp2)
    }

    if (is.null(age_max)) {
      age_max <- max(tmp["age"])
    }

    tmp <- tmp[tmp["age"] > age_min & tmp["age"] <= age_max, ]

    tmp$age <- as.factor(tmp$age)

    pg <- ggplot2::ggplot() +
      ggplot2::geom_line(
        ggplot2::aes(
          x = .data$x,
          y = .data$y,
          col = .data$age,
          linetype = .data$data_type
        ),
        data = tmp, size = 1.5
      ) +
      ggplot2::scale_color_viridis_d(end = 0.9) +
      ggplot2::labs(
        x = "destination area", y = "density",
        linetype = "data_type", color = "age", title = main
      ) +
      ggplot2::theme(text = ggplot2::element_text(size = 20))
  } else if (dim == 2) {
    ylim <- mark_recapture_object$destination$window$yrange

    kde_grid <- reshape::melt(kde[[b]]$z)[c(1:3, 5)]
    colnames(kde_grid) <- c("longitude", "latitude", "kde", "time")
    kde_grid$time <- as.numeric(kde_grid$time)
    kde_grid$data_type <- "estimated"

    if (b == "all") {
      if (true_values_available) {
        print("Not possible to plot true state for b = 'all'")
      }
    } else {
      if (true_values_available) {
        kde_grid_true <- numeric(4)
        grid_tmp <- expand.grid(
          longitude = longitude,
          latitude = latitude
        )
        for (t in 1:o_t) {
          tmp <- grid_tmp
          tmp$kde <- apply(grid_tmp, 1, function(x) {
            f_f(x,
              t = t, b = b,
              mark_recapture_object,
              p = p
            )
          })
          tmp$time <- t
          kde_grid_true <- rbind(kde_grid_true, tmp)
        }
        kde_grid_true <- kde_grid_true[-1, ]

        kde_grid_true$data_type <- "true"
        kde_grid <- as.data.frame(rbind(kde_grid, kde_grid_true))
      }
    }

    if (is.null(age_max)) {
      age_max <- max(kde_grid["time"])
    }

    kde_grid <- kde_grid[kde_grid["time"] > age_min &
      kde_grid["time"] <= age_max, ]

    if (log) {
      trans <- "log"
      my_breaks <- exp(seq(
        min(log(kde_grid$kde[kde_grid$kde != 0]),
          na.rm = TRUE
        ),
        max(kde_grid$kde, na.rm = TRUE),
        length.out = 7
      ))[2:6]
    } else {
      trans <- "identity"
      my_breaks <- seq(min(kde_grid$kde, na.rm = TRUE),
        max(kde_grid$kde, na.rm = TRUE),
        length.out = 7
      )[2:6]
    }

    pg <- ggplot2::ggplot() +
      ggplot2::ggtitle(paste(b)) +
      ggplot2::scale_fill_viridis_c("kde",
        trans = trans,
        breaks = my_breaks,
        labels = formatC(my_breaks,
          format = "e",
          digits = 1
        )
      ) +
      ggplot2::theme(text = ggplot2::element_text(size = 20))

    if (b == "all" || !true_values_available) {
      pg <- pg + ggplot2::geom_tile(
        data = kde_grid,
        ggplot2::aes(.data$longitude,
          .data$latitude,
          fill = .data$kde
        )
      )
    } else {
      pg <- pg + ggplot2::geom_tile(
        data = kde_grid,
        ggplot2::aes(.data$longitude,
          .data$latitude,
          fill = .data$kde
        )
      )
    }

    if (draw_boundaries) {
      pg <- pg +
        ggplot2::borders("world", colour = "grey30", size = 1) +
        ggplot2::coord_sf(
          xlim = xlim,
          ylim = ylim,
          expand = FALSE
        )
    }

    if (b != "all" && true_values_available) {
      pg <- pg + ggplot2::facet_wrap(data_type ~ time)
    } else {
      pg <- pg + ggplot2::facet_wrap(~time)
    }

    pg <- pg +
      ggplot2::coord_sf(
        expand = FALSE,
        crs = sf::st_crs(crs)
      )
  }

  if (pdf) {
    if (pdf) plot(pg)
    grDevices::dev.off()
  }
  pg
}
