#' survival estimator
#'
#' This function estimates the survival from a kernel density estimate of the data
#' of recovered individuals. It uses the data of all breeding areas at once.
#' @param res spatial resolution for longitude and latitude
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param dataType type of data. Simulated data: "sim", reals world data: "data".
#' Defaults to "sim".
#' @param robust type of estimator used for linear regression to estimate survival. If TRUE
#' a robust regression will be computed (robustbase::lmrob()), if FALSE an ordinary regression
#' (base::lm()). Defaults to TRUE.
#' @param auxiliaryVariable specify, if an auxiliary variable like the at risk-population should be
#' used. Defaults to NULL.
#' @return markRecaptureObject vector/matrix of length res with survival probabilities dependent on space
#' @export
#' @examples estS()

estS <- function(res,markRecaptureObject,dataType = "sim",
                 robust = TRUE, auxiliaryVariable = NULL){

  markRecaptureObject <- estLM(res,markRecaptureObject,
                               b="all",dataType = dataType,robust = robust,
                               auxiliaryVariable = auxiliaryVariable)
  markRecaptureObject$estimates[["s"]] <- exp(markRecaptureObject$estimates[["lm"]][["all"]][["slope"]])

  return(markRecaptureObject)
}
