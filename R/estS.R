#' survival estimator
#'
#' This function estimates the survival from a kernel density estimate of the data
#' of recovered individuals. It uses the data of all breeding areas at once.
#' @param res_x resolution in space, 1st dimension
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param res_y resolution in space, 2nd dimension. Defaults to res_x. Be careful
#' with changing as kde can only be estimated if res_x == res_y.
#' @param dataType type of data. Simulated data: "sim", reals world data: "data".
#' Defaults to "sim".
#' @param robust type of estimator used for linear regression to estimate survival. If TRUE
#' a robust regression will be computed (robustbase::lmrob()), if FALSE an ordinary regression
#' (base::lm()). Defaults to TRUE.
#' @param auxiliaryVariable specify, if an auxiliary variable like the at risk-population should be
#' used. Defaults to NULL.
#' @return markRecaptureObject vector/matrix of length res_x with survival probabilities dependent on space
#' @export
#' @examples estS()

estS <- function(res_x,markRecaptureObject,res_y = res_x,dataType = "sim",
                 robust = TRUE, auxiliaryVariable = NULL){

  markRecaptureObject <- estLM(res_x,markRecaptureObject,res_y = res_y,
                               b="all",dataType = dataType,robust = robust,
                               auxiliaryVariable = auxiliaryVariable)
  markRecaptureObject$estimates[["s"]] <- exp(markRecaptureObject$estimates[["lm"]][["all"]][["slope"]])

  return(markRecaptureObject)
}
