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

#' plot 3D plot using plotly
#'
#' This function plots the kernel density estimate and true density for
#' simulated data.
#' @param mark_recapture_object object of class mark_recapture_object
#' (see mark_recapture_object())
#' @param param parameter to be plotted. Can be "m" or "s". Only one parameter
#' can be plotted at once. Select different areas of origin with the argument
#' b.
#' @param b specifies the area of origin for which the plot is drawn. Can be
#' either a origin_name, the corresponding number of the area of origin or "all"
#' for all origins at once.
#' @param filename optional filename extension. Defaults to "".
#' @param no_ci suppresses drawing the confidence interval, even if bootstrap
#' information is in the mark_recapture_object. Defaults to FALSE.
#' @importFrom dplyr %>%
#' @return saves a html file and returns the plotly plot
#' @export
#' @examples{
#' mro2 <- est_uncertainty(mro2D, "s", iterations = 2, filename = "test")
#' mro2 <- bootstrap_quantiles(mro2, "s", "test")
#' plotly_param(mro2, param = "s")
#' }
#'
plotly_param <- function(mark_recapture_object, param, b = "all",
                         filename = "", no_ci = FALSE) {
  est <- mark_recapture_object$estimates[[param]]

  if (is.null(est)) {
    stop("Error: mark_recapture_object$estimates[[param]] is NULL. Did you
         estimate parameters first? Use, e.g., est_param.")
  }

  if (param == "m") {
    est <- est[[b]]
  }

  bootstrap <- mark_recapture_object$estimates$bootstrap$bootstrap_quantiles %>%
    dplyr::filter(
      .data$parameter == param,
      .data$mark_area == b
    )
  if (!no_ci && !is.null(bootstrap)) {
    if (rlang::is_installed("tidyr")) {
      upper_bootstrap_quantile <- bootstrap %>%
        dplyr::select(.data$longitude, .data$latitude, .data$uq) %>%
        tidyr::pivot_wider(names_from = .data$latitude, values_from = .data$uq)

      upper_bootstrap_quantile <- t(
        upper_bootstrap_quantile[, 3:ncol(upper_bootstrap_quantile)]
      )

      lower_bootstrap_quantile <- bootstrap %>%
        dplyr::select(.data$longitude, .data$latitude, .data$lq) %>%
        tidyr::pivot_wider(names_from = .data$latitude, values_from = .data$lq)
    } else {
      rlang::check_installed("tidyr")
    }

    lower_bootstrap_quantile <- t(
      lower_bootstrap_quantile[, 3:ncol(lower_bootstrap_quantile)]
    )
  }

  if (rlang::is_installed("plotly")) {
    param_fig <- plotly::plot_ly(showscale = FALSE) %>%
      plotly::add_surface(z = ~est) %>%
      plotly::layout(scene = list(
        xaxis = list(title = "longitude"),
        yaxis = list(title = "latitude"),
        zaxis = list(title = param)
      ))

    if (!no_ci && !is.null(bootstrap)) {
      param_fig <- param_fig %>%
        plotly::add_surface(
          z = ~upper_bootstrap_quantile, opacity = 0.5,
          colorscale = list(c(0, 1), c("grey", "grey"))
        ) %>%
        plotly::add_surface(
          z = ~lower_bootstrap_quantile, opacity = 0.5,
          colorscale = list(c(0, 1), c("grey", "grey"))
        )
    }
  } else {
    rlang::check_installed("plotly")
  }

  if (rlang::is_installed("htmlwidgets")) {
    htmlwidgets::saveWidget(param_fig,
      paste(param, "_", filename, ".html", sep = ""),
      selfcontained = TRUE
    )
  } else {
    rlang::check_installed("htmlwidgets")
  }


  param_fig
}
