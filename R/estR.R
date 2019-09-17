#' recovery probability estimator
#'
#' This function estimates the recovery probability uniformely over the whole recovery area.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param b name of breeding area to estimate recovery for. Defaults to "all".
#' @return scalar of recovery probability
#' @export
#' @examples estR()
#'

estR <- function(markRecaptureObject, b = "all"){

  k <- markRecaptureObject$breedingAreas[[b]]$numberOfRecoveries
  c <- markRecaptureObject$estimates$c[[b]]
  n <- markRecaptureObject$breedingAreas[[b]]$markedInds

  markRecaptureObject$estimates[["r"]] <- c*k/n
  markRecaptureObject
}
