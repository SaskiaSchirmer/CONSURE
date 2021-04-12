#' survival estimator
#'
#' This function estimates the survival from a kernel density estimate of the data
#' of recovered individuals. It uses the data of all breeding areas at once.
#' @param res spatial resolution for longitude and latitude
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param robust type of estimator used for linear regression to estimate survival. If TRUE
#' a robust regression will be computed (robustbase::lmrob()), if FALSE an ordinary regression
#' (base::lm()). Defaults to TRUE.
#' @param auxiliaryVariable specify, if an auxiliary variable like the at risk-population should be
#' used. Defaults to NULL.
#' @return markRecaptureObject vector/matrix of length res with survival probabilities dependent on space
#' @export
#' @examples estS()

estS <- function(markRecaptureObject,
                 auxiliaryVariable = NULL){

  robust <- markRecaptureObject$robust

  markRecaptureObject <- estLM(markRecaptureObject,
                               b="all",
                               auxiliaryVariable = auxiliaryVariable)
  markRecaptureObject$estimates[["s"]] <- exp(markRecaptureObject$estimates[["lm"]][["all"]][["slope"]])

  return(markRecaptureObject)
}
