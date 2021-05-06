#' subdensity function for recovered individuals
#'
#' This function sets up the subdensity function for every spatiotemporal point for recovered individuals
#' by means of given survival, migratory connectivity and recovery probability.
#' @param t integer: temporal component of recovery
#' @inheritParams f_nf_sub
#' @return subdensity of recovered individuals for the specified parameters
#' @export
#' @examples ffs <- f_f_sub(1,1,1,mro1D)
f_f_sub <- function(w,t,b,markRecaptureObject){
  r <- markRecaptureObject$winteringArea$recovery
  s <- markRecaptureObject$winteringArea$survival
  m <- markRecaptureObject$breedingAreas[[b]]$migratoryConnectivity

  m(w)*(s(w)^(t-1)*(1-s(w))*r(w))
}
