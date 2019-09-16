#' survival estimator
#'
#' This function estimates the survival from a kernel density estimate of the data
#' of recovered individuals. It uses the data of all breeding areas at once.
#' @param res_x resolution in space
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @return vector/matrix of length res_x with survival probabilities dependent on space
#' @export
#' @examples estS()

estS <- function(res_x,markRecaptureObject,res_y = res_x,dataType = "sim",robust = T){

  markRecaptureObject <- estLM(res_x,markRecaptureObject,res_y = res_y,b="all",dataType,robust)
  markRecaptureObject$estimates[["s"]] <- exp(markRecaptureObject$estimates[["lm"]][["all"]][["slope"]])

  return(markRecaptureObject)
}
