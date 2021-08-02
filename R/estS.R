#' survival estimator
#'
#' This function estimates the survival from a kernel density estimate of the
#' data of recovered individuals. It uses the data of all breeding areas at
#' once.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @return markRecaptureObject vector/matrix of length res with survival
#' probabilities dependent on space
#' @export
#' @examples mro <- estS(mro1D)
estS <- function(markRecaptureObject) {
  markRecaptureObject <- estLM(markRecaptureObject,
    b = "all"
  )
  markRecaptureObject$estimates[["s"]] <-
    exp(markRecaptureObject$estimates[["lm"]][["all"]][["slope"]])

  return(markRecaptureObject)
}
