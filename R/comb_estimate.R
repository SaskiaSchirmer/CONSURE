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

#' Function to get combined migratory connectivity estimate.
#'
#' This function uses an optimization procedure to correct the continuous
#' migratory connectivity estimator by the discrete migratory connectivity
#' estimator. Throws an error if convergence is not achieved.
#' @param optimization_object an object of the class 'optimization_object' (see
#' constructors)
#' @param start_times number of repetitions of the optimization procedure.
#' Defaults to 1.
#' @param maxit numbers of iterations used by the control argument of optim,
#' defaults to 10000, see ?optim for details.
#' @param reltol Relative convergence tolerance to stop optimization algorithm.
#' Used by the control argument of optim,
#' defaults to 1e-8, see ?optim for details.
#' @param change_r logical. If TRUE, it estimates the combined version of the
#' recovery probability and adds it to mark_recapture_object$estimates$r_combined.
#' Should be commonly set to TRUE if the breeding area argument in the
#' optimization Object is 'all' and FALSE otherwise. Defaults to FALSE.
#'
#' @return f_x data.frame with estimates of every optimization
#' @return val numeric vector of all optimizations
#' @return mark_recapture_object
#'
#' @export
#' @examples{
#'     oO <- optimization_object(mark_recapture_object = mro1D_increasing$mro,
#'         b = "all",
#'         split = mro1D_increasing$split,
#'         lambda  = c(.05,300))
#'
#'     mro <- combEstimate(optimization_object = oO)
#' }

comb_estimate <- function(optimization_object,
                         start_times = 1,
                         maxit = 100000,
                         reltol = 1e-8,
                         change_r = FALSE) {
  mark_recapture_object <- optimization_object$mark_recapture_object
  dim <- mark_recapture_object$spatial_dimension

  if (dim == 1) {
    inside <- colSums(optimization_object$inside) > 0
  } else {
    inside <- optimization_object$inside
  }

  normalize <- sum(inside, na.rm = TRUE)

  f_x <- matrix(NA,
    ncol = start_times,
    nrow = dim(optimization_object$raw_spline)[1]
  )
  val <- numeric(start_times)
  convergence <- numeric(start_times)

  for (i in 1:start_times) {
    print(paste("startCombEst", i))
    opt_beta <- stats::optim(
      par = optimization_object$init_beta(),
      method = "BFGS",
      fn = optimization_object$penalize,
      gr = optimization_object$gradient,
      control = list(
        maxit = maxit,
        reltol = reltol
      )
    )

    print(opt_beta$value)
    print(paste("optimization done", i))

    if (opt_beta$convergence != 0) {
      message("convergence not achieved")

      h_x <- (optimization_object$raw_spline %*% opt_beta$par)
      h_x[!inside] <- NA
      f_x[, i] <- exp(h_x) / sum(exp(h_x), na.rm = TRUE) * normalize

      val[i] <- NA
    } else {
      h_x <- (optimization_object$raw_spline %*% opt_beta$par)
      h_x[!inside] <- NA
      f_x[, i] <- exp(h_x) / sum(exp(h_x), na.rm = TRUE) * normalize
      val[i] <- opt_beta$value
      convergence[i] <- (opt_beta$convergence == 0)
    }
  }

  if (sum(convergence) != 0) {
    f <- mark_recapture_object$estimates$m_combined[[optimization_object$b]] <-
      matrix(f_x[, which.min(val)],
        ncol = dim(mark_recapture_object$estimates$s)[2]
      )

    if (change_r) {
      mark_recapture_object$estimates$r_combined <- matrix(
        exp(
          mark_recapture_object$estimates$lm[[optimization_object$b]]$intercept
        ) /
          f /
          (1 - mark_recapture_object$estimates$s) *
 mark_recapture_object$origins[[optimization_object$b]]$number_of_recoveries /
      mark_recapture_object$origins[[optimization_object$b]]$marked_individuals,
      ncol = dim(mark_recapture_object$estimates$s)[2]
    )

      origin_names <- names(mark_recapture_object$origins)

      for (b in origin_names) {
        mark_recapture_object$estimates$m_corrected[[b]] <- exp(
          mark_recapture_object$estimates$lm[[b]]$intercept
        ) /
          mark_recapture_object$estimates$r_combined /
          (1 - mark_recapture_object$estimates$s) *
          mark_recapture_object$origins[[b]]$number_of_recoveries /
          mark_recapture_object$origins[[b]]$marked_individuals

        mark_recapture_object$estimates$m_corrected[[b]] <-
          mark_recapture_object$estimates$m_corrected[[b]] /
            sum(mark_recapture_object$estimates$m_corrected[[b]], na.rm = TRUE) *
            normalize
      }
    }

    return(list(
      f_x = f_x, val = val,
      mark_recapture_object = mark_recapture_object,
      convergence = convergence
    ))
  } else {
    message("convergence never achieved")
  }
}
