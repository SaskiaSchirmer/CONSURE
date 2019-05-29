#' subdensity function for recovered individuals
#'
#' This function sets up the subdensity function for every spatiotemporal point for recovered individuals
#' by means of given survival, migratory connectivity and recovery probability.
#' @param w decimal number: spatial point of recovery (1-dimensional)
#' @param t integer: temporal component of recovery
#' @param b integer: breeding area
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @return subdensity of recovered individuals for the specified parameters
#' @export
#' @examples f_f_sub()
f_f_sub <- function(w,t,b,markRecaptureObject){
  r <- markRecaptureObject$winteringArea$recovery
  s <- markRecaptureObject$winteringArea$survival
  m <- markRecaptureObject$breedingAreas[[b]]$migratoryConnectivity

  m(w)*(s(w)^(t-1)*(1-s(w))*r())
}
