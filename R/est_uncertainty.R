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

#' linear model of kernel density estimate
#'
#' This function estimates the uncertainty of the parameters survival, migratory
#' connectivity and recovery probability by bootstrapping the marking data.
#' @inheritParams est_s
#' @param parameters vector of characters specifying the parameter names
#' considered for uncertainty estimation, possible values are s for survival,
#' m for migratory connectivity and r for recovery probability. Defaults to
#' c("s","m","r").
#' @param iterations numeric. Number of bootstraps performed. Defaults to NULL.
#' @param bootstrap_data data prepared by bootstrapMarkingData, offers the
#' option to compute bootstrap in parallel. Defauls to NULL.
#' @param res spatial resolution. Defaults to NULL. If NULL, resolution of the
#' mark_recapture_object$spatial_resolution is used.
#' @param filename (path to file) and filename to store the
#' mark_recapture_object with bootstrap estimates.
#' @importFrom dplyr %>%
#'
#' @return  mark_recapture_object with added bootstrap uncertainty for
#' parameters
#' @export
#' @examples{
#' mro  <- mro1D
#' mro <- est_uncertainty(mro, parameters = "s",
#' iterations = 1, filename = "test")
#' }
#'
est_uncertainty <- function(mark_recapture_object,
                            parameters = c("s", "m", "r"),
                            iterations = NULL, bootstrap_data = NULL,
                            res = NULL,
                            filename) {
  if (is.null(res)) {
    if (!is.null(mark_recapture_object$spatial_resolution)) {
      res <- mark_recapture_object$spatial_resolution
    } else {
      message("Please provide a spatial resolution in `res`.")
    }
  } else {
    mark_recapture_object$spatial_resolution <- res
  }

  if (is.null(bootstrap_data)) {
    ls <- vector(mode = "list", length = iterations)

    out <- lapply(ls, FUN = function(x) {
      tmp <- bootstrap_marking_data(mark_recapture_object)
      tmp <- est_parameters(tmp)
      list(
        s = tmp$estimates$s,
        m = tmp$estimates$m,
        r = tmp$estimates$r,
        lm = tmp$estimates$lm,
        kde = tmp$kde
      )
    })
  } else {
    ls <- bootstrap_data

    out <- lapply(ls, FUN = function(x) {
      if (is.null(res)) {
        res <- mark_recapture_object$spatial_resolution
      }

      tmp <- est_parameters(x$data, res = res)
      list(
        s = tmp$estimates$s,
        m = tmp$estimates$m,
        r = tmp$estimates$r,
        lm = tmp$estimates$lm,
        kde = tmp$kde
      )
    })
  }

  mark_recapture_object$estimates$bootstrap$raw_bootstrap <- out
  save(mro = mark_recapture_object, file = paste(filename, ".Rdata",
    sep = ""
  ))

  return(mark_recapture_object)
}
