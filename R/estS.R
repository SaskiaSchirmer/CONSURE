#' survival estimator
#'
#' This function estimates the survival from a kernel density estimate of the data
#' of recovered individuals. It uses the data of all breeding areas at once.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param auxiliaryVariable specify, if an auxiliary variable like the at risk-population should be
#' used. Defaults to NULL.
#' @return markRecaptureObject vector/matrix of length res with survival probabilities dependent on space
#' @export
#' @examples mro <- estS(mro1D)

estS <- function(markRecaptureObject,
                 auxiliaryVariable = NULL){

  robust <- markRecaptureObject$robust

  markRecaptureObject <- estLM(markRecaptureObject,
                               b="all",
                               auxiliaryVariable = auxiliaryVariable)
  markRecaptureObject$estimates[["s"]] <- exp(markRecaptureObject$estimates[["lm"]][["all"]][["slope"]])

  return(markRecaptureObject)
}
