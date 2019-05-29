#' density function for recovered individuals only
#'
#' This function sets up a valid density function for every spatiotemporal point for recovered individuals
#' by means of given survival, migratory connectivity and recovery probability.
#' @param w decimal number: spatial point of recovery (1-dimensional)
#' @param t integer: temporal component of recovery
#' @param b integer: breeding area
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param p desimal: complementary probability to be not found
#' @return density of recovered individuals for the specified parameters
#' @export
#' @examples f_f()
f_f <- function(w,t,b,markRecaptureObject,p){
  f_f_sub(w,t,b,markRecaptureObject)/p
}
