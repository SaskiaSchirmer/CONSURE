#' Plot function for true and estimated continuous, discrete and combined
#' migratory connectivity
#'
#' This function plots (in 1D) combinations of true and estimated migratory
#' connectivity values. It is possible to chose between continuous, discrete
#' and combined estimates.
#'
#' @param mark_recapture_object object of class mark_recapture_object
#' @param optimization_object object of class optimization_object
#' @param pdf logical, plot as pdf?, defaults to FALSE
#' @param file_name character string, only if pdf is TRUE, defaults to current
#'                 time
#' @param true_continuous logical, plot true continuous migratory connectivity
#' @param true_discrete logical, plot true discrete migratory connectivity
#' @param est_continuous logical, plot estimated continuous migratory
#'                      connectivity
#' @param est_discrete logical, plot estimated discrete migratory connectivity
#' @param est_combined logical, plot estimated combined migratory connectivity
#' @param est_corrected logical, plot estimated corrected mgiratory connectivity,
#'     defaults to FALSE
#' @param zlim limits for migratory connectivity
#' @param draw_boundaries logical, specifies if the boundaries of the world map
#'    should be drawn, defaults to FALSE
#' @param true_values_available logical, use TRUE for simulated data, FALSE for
#'                            real-world data. Defaults to FALSE.
#' @param uq upper quantile, similar to zlim?
#' @importFrom rlang .data
#'
#' @export
#' @examples{
#'     o_o <- optimization_object(mark_recapture_object = mro1D_increasing$mro,
#'         b = "all",
#'         split = mro1D_increasing$split,
#'         lambda  = c(.05, 300))
#'
#'     plotCombM(mro1D_increasing$mro, o_o)
#' }

plot_comb_m <- function(mark_recapture_object,
                      optimization_object,
                      pdf = FALSE,
                      file_name = paste("m_comb_", Sys.time(), ".pdf", sep = ""),
                      true_continuous = TRUE,
                      true_discrete = TRUE,
                      est_continuous = TRUE,
                      est_discrete = FALSE,
                      est_combined = TRUE,
                      est_corrected = FALSE,
                      zlim = NULL,
                      draw_boundaries = FALSE,
                      true_values_available = FALSE,
                      uq = 1) {
  b <- optimization_object$b
  prop <- mark_recapture_object$origins[[b]]$m_discrete
  dim <- mark_recapture_object$spatial_dimension
  m_combined <- mark_recapture_object$estimates$m_combined[[b]]
  m_continuous <- mark_recapture_object$estimates$m[[b]]
  m_corrected <- mark_recapture_object$estimates$m_corrected[[b]]
  res <- mark_recapture_object$spatial_resolution
  if (true_continuous) {
    m_func <- mark_recapture_object$origins[[b]]$migratory_connectivity
  }
  xlim <- mark_recapture_object$destination$window$xrange
  ylim <- mark_recapture_object$destination$window$yrange
  y_help <- optimization_object$y
  split <- unname(table(optimization_object$split))

  if (pdf) pdf(file_name, width = 5.5)

  pl <- ggplot2::ggplot()

  if (dim == 1) {
    if (true_continuous) {
      pl <- pl + ggplot2::stat_function(
        data = data.frame(x = y_help),
        ggplot2::aes(color = "true"),
        fun = function(x) {
          dat <- as.data.frame(matrix(x, ncol = 1))
          apply(dat, 1, m_func)
        }
      )
    }

    if (true_discrete) {
      pl <- pl + ggplot2::geom_line(data.frame(
        x = y_help,
        y = rep(prop / split, times = split)
      ),
      mapping = ggplot2::aes(x = .data$x, y = .data$y, color = "discrete true")
      )
    }

    if (est_discrete) {
      pl <- pl

      message("Discrete estimates cannot be plotted at the moment.")
    }


    if (est_continuous) {
      pl <- pl + ggplot2::geom_line(data.frame(
        x = y_help,
        y = c(m_continuous)
      ),
      mapping = ggplot2::aes(
        x = .data$x, y = .data$y,
        color = "continuous estimate"
      ), size = 1.5
      )
    }

    if (est_corrected) {
      pl <- pl + ggplot2::geom_line(data.frame(
        x = y_help,
        y = c(m_corrected)
      ),
      mapping = ggplot2::aes(
        x = .data$x, y = .data$y,
        color = "corrected estimate"
      )
      )
    }

    if (est_combined) {
      pl <- pl + ggplot2::geom_line(data.frame(
        x = y_help,
        y = c(m_combined)
      ),
      mapping = ggplot2::aes(
        x = .data$x, y = .data$y,
        color = "combined estimate"
      ),
      size = 1.5
      )
    }

    pl <- pl +
      ggplot2::ylab("migratory connectivity") +
      ggplot2::xlab("wintering area") +
      ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        text = ggplot2::element_text(size = 20)
      ) +
      ggplot2::scale_color_manual("",
        breaks = c(
          "true", "discrete true", "discrete estimate",
          "continuous estimate", "corrected estimate",
          "combined estimate"
        ),
        values = c("black", "grey", "green", "#009E73", "#E69F00", "#D55E00")
      )

    if (!is.null(zlim)) pl <- pl + ggplot2::lims(y = zlim)
  } else if (dim == 2) {
    m_grid <- reshape::melt(m_combined)
    m_grid$X1 <- rep(y_help$latitude, res)
    m_grid$X2 <- rep(y_help$longitude, each = res)
    m_grid$data_type <- "combined"

    tmp <- reshape::melt(m_continuous)
    tmp$X1 <- rep(y_help$latitude, res)
    tmp$X2 <- rep(y_help$longitude, each = res)
    tmp$data_type <- "continuous"

    m_grid <- as.data.frame(rbind(m_grid, tmp))
    m_grid$origin <- b
    m_grid <- m_grid[, c(2, 1, 3, 5, 4)]

    colnames(m_grid) <- c(
      "longitude", "latitude", "m", "origin",
      "data_type"
    )
    if (true_continuous) {
      m_grid_true <- expand.grid(
        longitude = seq(xlim[1], xlim[2], length.out = res),
        latitude = seq(ylim[1], ylim[2], length.out = res),
        origin = b
      )
      m_grid_true$m <- apply(m_grid_true, 1, function(x) {
        m_func(as.numeric(x[1:2]))
      })
      m_grid_true <- m_grid_true[, c(1, 2, 4, 3)]
      m_grid_true$data_type <- "true"

      m_grid <- as.data.frame(rbind(m_grid, m_grid_true))
    }

    my_breaks <- stats::quantile(m_grid$m, seq(0, 1, length.out = 11),
      na.rm = TRUE
    )
    my_breaks <- seq(0, max(m_grid$m, na.rm = TRUE), length.out = 11)
    my_breaks <- seq(0, stats::quantile(m_grid$m, uq, na.rm = TRUE),
      length.out = 11
    )

    m_grid$data_type <- factor(m_grid$data_type, levels = c(
      "true", "continuous",
      "combined"
    ))

    pl <- pl +
      ggplot2::labs(fill = "estimated\n migratory\n connectivity") +
      ggplot2::scale_fill_viridis_c("connectivity",
        values = scales::rescale(my_breaks),
        trans = "identity", limits = c(max(0, my_breaks[1]), my_breaks[11]),
        breaks = seq(max(0, my_breaks[1]), my_breaks[11], length.out = 5),
        labels = formatC(seq(max(0, my_breaks[1]),
          my_breaks[11],
          length.out = 5
        ), format = "e", digits = 1)
      ) +
      ggplot2::theme(text = ggplot2::element_text(size = 24))

    pl <- pl + ggplot2::facet_grid(data_type ~ .)

    if (!true_values_available) {
      pl <- pl + ggplot2::geom_tile(
        data = m_grid,
        ggplot2::aes(.data$longitude,
          .data$latitude,
          fill = .data$m
        )
      )
    } else {
      pl <- pl + ggplot2::geom_tile(
        data = m_grid, ggplot2::aes(.data$longitude,
          .data$latitude,
          fill = .data$m
        ),
        height = 1 / res, width = 1 / res
      )
    }

    if (draw_boundaries) {
      pl <- pl +
        ggplot2::borders("world", colour = "grey30", size = 1) +
        ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE)
    }
  } else {
    message("plot for number of dimension unavailable")
  }

  plot(pl)
  if (pdf) grDevices::dev.off()

  return(pl)
}
