#' subdensity function for not seen individuals
#'
#' This function sets up the subdensity function for every spatiotemporal point
#' for not seen individuals by means of given survival, migratory connectivity
#' and recovery probability. Not seen individuals died in observation time but
#' were not found or survived observation time.
#' @param w decimal number: spatial point of recovery (1-dimensional)
#' @param b integer: breeding area
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @return subdensity of not seen individuals for the specified parameters
#' @export
#' @examples f_nf_sub()
f_nf_sub <- function(w,b,markRecaptureObject){
  r <- markRecaptureObject$winteringArea$recovery
  s <- markRecaptureObject$winteringArea$survival
  m <- markRecaptureObject$breedingAreas[[b]]$migratoryConnectivity
  T <- markRecaptureObject$observationTime

  ((1-r(w))*(1-s(w)^T)+s(w)^T)*m(w)
}
