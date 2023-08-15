#' Plot function for true and estimated continuous, discrete and combined
#' recovery probability
#'
#' This function plots (in 1D) combinations of true and estimated recovery
#' probability values. It is possible to chose between continuous, discrete and
#' combined estimates.
#'
#' @param mark_recapture_object object of class mark_recapture_object
#' @param optimization_object object of class optimization_object
#' @param pdf logical, plot as pdf?, defaults to FALSE
#' @param file_name character string, only if pdf is TRUE, defaults to current
#'                 time
#' @param true_continuous logical, plot true continuous recovery probability
#' @param true_discrete logical, plot true discrete recovery probability
#' @param est_continuous logical, plot estimated continuous recovery probability
#' @param est_discrete logical, plot estimated discrete recovery probability
#' @param est_combined logical, plot estimated combined recovery probability
#' @param draw_boundaries logical, specifies if the boundaries of the world map
#'                       should be drawn, defaults to FALSE
#' @param true_values_available logical, use TRUE for simulated data, FALSE for
#'                            real-world data. Defaults to FALSE.
#' @importFrom rlang .data
#'
#' @export
#' @examples{
#'     o_o <- optimization_object(mark_recapture_object = mro1D_increasing$mro,
#'         b = "all",
#'         split = mro1D_increasing$split,
#'         lambda  = c(.05,300))
#'
#'     tmp <- comb_estimate(o_o, start_times = 10, maxit = 100000,
#'                          reltol = 1e-8,
#'                          change_r = TRUE)
#'
#'     plot_comb_r(tmp$mark_recapture_object, o_o)
#' }
plot_comb_r <- function(mark_recapture_object,
                      optimization_object,
                      pdf = FALSE,
                      file_name = paste("rComb_", Sys.time(), ".pdf", sep = ""),
                      true_continuous = TRUE,
                      true_discrete = TRUE,
                      est_continuous = TRUE,
                      est_discrete = FALSE,
                      est_combined = TRUE,
                      draw_boundaries = FALSE,
                      true_values_available = FALSE) {
  b <- optimization_object$b
  dim <- mark_recapture_object$spatial_dimension
  r_combined <- mark_recapture_object$estimates$r_combined
  r_continuous <- mark_recapture_object$estimates$r
  res <- mark_recapture_object$spatial_resolution
  if (true_continuous) r_func <- mark_recapture_object$destination$recovery
  xlim <- mark_recapture_object$destination$window$xrange
  ylim <- mark_recapture_object$destination$window$yrange
  y_help <- optimization_object$y
  crs <- mark_recapture_object$destination$crs

  if (pdf) pdf(file_name, width = 5.5)

  pl <- ggplot2::ggplot()

  if (dim == 1) {
    if (true_continuous) {
      pl <- pl + ggplot2::stat_function(
        data = data.frame(x = seq(0, 1, le = 100)),
        ggplot2::aes(color = "true"),
        fun = function(x) r_func(x)
      )
    }

    if (true_discrete) message("True discrete values cannot be plotted in the
                             moment.")

    if (est_discrete) message("Estimated discrete values cannot be plotted in the
                             moment.")


    if (est_combined) {
      pl <- pl + ggplot2::geom_line(data.frame(
        x = y_help,
        y = c(r_combined)
      ),
      mapping = ggplot2::aes(
        x = .data$x, y = .data$y,
        color = "combined estimate"
      ),
      size = 1.5
      )
    }

    if (est_continuous) {
      pl <- pl + ggplot2::geom_hline(ggplot2::aes(
        yintercept = r_continuous,
        color = "continuous estimate"
      ),
      size = 1.5
      )
    }

    pl <- pl +
      ggplot2::ylab("recovery probability") +
      ggplot2::xlab("destination area") +
      ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        text = ggplot2::element_text(size = 20)
      ) +
      ggplot2::scale_color_manual("",
        breaks = c(
          "true", # "discrete true",
          # "discrete estimate",
          "continuous estimate", "combined estimate"
        ),
        values = c(
          "black", # "grey",
          # "green",
          "#009E73", "#E69F00"
        )
      )
  } else if (dim == 2) {
    r_grid <- reshape::melt(r_combined)
    r_grid$X1 <- rep(y_help$longitude, each = res)
    r_grid$X2 <- rep(y_help$latitude, res)
    r_grid$data_type <- "combined"

    tmp <- matrix(rep(r_continuous, res * res))
    tmp[!mark_recapture_object$inside] <- NA
    tmp <- as.data.frame(tmp)
    tmp$X1 <- rep(y_help$longitude, each = res)
    tmp$X2 <- rep(y_help$latitude, res)
    tmp$data_type <- "continuous"
    colnames(tmp) <- c("value", "X1", "X2", "data_type")
    tmp <- tmp[, c(2, 3, 1, 4)]

    r_grid <- as.data.frame(rbind(r_grid, tmp))
    r_grid$origin <- b
    r_grid <- r_grid[, c(1, 2, 3, 5, 4)]

    colnames(r_grid) <- c("longitude", "latitude", "r", "origin",
                         "data_type")
    if (true_continuous) {
      r_grid_true <- expand.grid(
        longitude = seq(xlim[1], xlim[2], length.out = res),
        latitude = seq(ylim[1], ylim[2], length.out = res),
        origin = b
      )
      r_grid_true$r <- apply(r_grid_true, 1, function(x) {
        r_func(as.numeric(x[1:2]))
      })
      r_grid_true <- r_grid_true[, c(1, 2, 4, 3)]
      r_grid_true$data_type <- "true"

      r_grid <- as.data.frame(rbind(r_grid, r_grid_true))
      r_grid$data_type <- factor(r_grid$data_type,
        levels = c("true", "continuous", "combined")
      )
    } else {
      r_grid$data_type <- factor(r_grid$data_type,
        levels = c("continuous", "combined")
      )
    }

    my_breaks <- stats::quantile(r_grid$r, seq(0, 1, length.out = 11),
      na.rm = TRUE
    )
    my_breaks <- seq(0, stats::quantile(r_grid$r, 1, na.rm = TRUE),
      length.out = 11
    )

    if(rlang::is_installed("scales")) {
      pl <- pl +
        ggplot2::labs(fill = "estimated\n recovery\n probability") +
        ggplot2::scale_fill_viridis_c("recovery",
                                      values = scales::rescale(my_breaks),
                                      trans = "identity", limits = range(my_breaks),
                                      breaks = seq(my_breaks[1], my_breaks[11], length.out = 5),
                                      labels = formatC(seq(my_breaks[1], my_breaks[11], length.out = 5),
                                                       format = "e", digits = 1
                                      )
        ) +
        ggplot2::theme(text = ggplot2::element_text(size = 24))
    } else {
      rlang::check_installed("scales")
    }



    pl <- pl + ggplot2::facet_grid(data_type ~ .)

    if (!true_values_available) {
      pl <- pl + ggplot2::geom_tile(data = r_grid, ggplot2::aes(.data$longitude,
        .data$latitude,
        fill = .data$r
      ))
    } else {
      pl <- pl + ggplot2::geom_tile(
        data = r_grid, ggplot2::aes(.data$longitude,
          .data$latitude,
          fill = .data$r
        ),
        height = 1 / res, width = 1 / res
      )
    }

    if (draw_boundaries) {
      pl <- pl +
        ggplot2::borders("world", colour = "grey30", size = 1) +
        ggplot2::coord_sf(
          xlim = xlim, ylim = ylim, expand = FALSE,
          crs = sf::st_crs(crs)
        )
    }
  } else {
    message("plot for number of dimension unavailable")
  }


  plot(pl)
  if (pdf) grDevices::dev.off()

  return(pl)
}
