#' density function for recovered individuals only
#'
#' This function sets up a valid density function for every spatiotemporal point for recovered individuals
#' by means of given survival, migratory connectivity and recovery probability.
#' @inheritParams f_f_sub
#' @param p decimal: complementary probability to be not found
#' @return density of recovered individuals for the specified parameters
#' @export
#' @examples{
#'     p <- 1-p_nf(b=1,mro1D)
#'     ff <- f_f(1,1,1,mro1D,p)
#' }
f_f <- function(w,t,b,markRecaptureObject,p){
  f_f_sub(w,t,b,markRecaptureObject)/p
}
