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
#' This function estimates the parameters of a linear model for a certain point
#' of the kernel density estimate dependent on time. Linear models are obtained
#' according to the resolution.
#' @inheritParams est_s
#' @param b name of areas of origin
#' @param fixed_slope numeric. Value for the fixed slope, e.g., to estimate
#' a linear model for the areas of origin separately.
#' @return vector of length res with survival probabilities dependent on space
#' @export
#' @examples mro <- est_lm(mro1D, b = "all")
est_lm <- function(mark_recapture_object, b,
                   fixed_slope = NULL) {
  res <- mark_recapture_object$spatial_resolution
  robust <- mark_recapture_object$robust
  dim <- mark_recapture_object$spatial_dimension
  kde_all <- mark_recapture_object$kde[[b]]$z

  kde_all <- lapply(
    kde_all, function(x) {
      unname(x$v)
    }
  )

  if (dim == 1) {
    kde_all <- sapply(kde_all, function(x) colMeans(x))
    res_y <- 1
  } else if (dim == 2) {
    kde_all <- sapply(kde_all, function(x) x)
    res_y <- res
  }

  if (sum(is.infinite(kde_all)) > 0) {
    warning("Infinite values in kernel density estimate.")
  }
  kde_all[is.infinite(kde_all)] <- NA

  lm_fit <- apply(cbind(kde_all, slope = c(fixed_slope)), 1, function(x) {
    if (sum(x, na.rm = TRUE) != 0) {
      age <- as.numeric(colnames(kde_all)) - 1

      if (robust) {
        if (is.null(fixed_slope)) {
          kde_values <- log(x + 10^-200)
          if (rlang::is_installed("robustbase")) {
            fit <- try(
              robustbase::lmrob(kde_values ~ age, setting = "KS2014"),
              silent = TRUE
            )
          } else {
            rlang::check_installed("robustbase")
          }
        } else {
          kde_values <- log(x[-length(x)] + 10^-200)
          fixed_slope <- log(x["slope"])
          if (rlang::is_installed("robustbase")) {
            fit <- try(robustbase::lmrob(kde_values ~ 1 +
              offset(fixed_slope * age),
            setting = "KS2014"
            ), silent = TRUE)
          } else {
            rlang::check_installed("robustbase")
          }
        }
      } else {
        if (is.null(fixed_slope)) {
          kde_values <- log(x + 10^-200)
          fit <- stats::lm(kde_values ~ age)
        } else {
          kde_values <- log(x[-length(x)] + 10^-200)
          fixed_slope <- log(x["slope"])
          fit <- stats::lm(kde_values ~ 1 + offset(fixed_slope * age))
        }
      }
      if (is.null(fixed_slope)) {
        tryCatch(c(stats::coefficients(fit), summary(fit)$r.squared),
          error = function(e) {
            c(NA, NA, NA)
          }
        )
      } else {
        tryCatch(c(stats::coefficients(fit)[1], NA, NA),
          error = function(e) {
            c(NA, NA, NA)
          }
        )
      }
    } else {
      c(NA, NA, NA)
    }
  })

  mark_recapture_object$estimates[["lm"]][[b]][["intercept"]] <-
    matrix(lm_fit[1, ], ncol = res, nrow = res_y)

  if (is.null(fixed_slope)) {
    mark_recapture_object$estimates[["lm"]][[b]][["slope"]] <-
      matrix(lm_fit[2, ], ncol = res, nrow = res_y)
    mark_recapture_object$estimates[["lm"]][[b]][["gof"]] <-
      matrix(lm_fit[3, ], ncol = res, nrow = res_y)
  } else {
    mark_recapture_object$estimates[["lm"]][[b]][["slope"]] <-
      matrix(fixed_slope, ncol = res, nrow = res_y)
    mark_recapture_object$estimates[["lm"]][[b]][["gof"]] <-
      matrix(NA, ncol = res, nrow = res_y)
  }

  return(mark_recapture_object)
}
