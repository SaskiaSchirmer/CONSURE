#' subdensity function for recovered individuals
#'
#' This function sets up the subdensity function for every spatiotemporal point for recovered individuals
#' by means of given survival, migratory connectivity and recovery probability.
#' @param w decimal number: spatial point of recovery (1-dimensional)
#' @param t integer: temporal component of recovery
#' @param b integer: breeding area
#' @param s function: survival function defined over whole wintering area, independent of breeding area
#' @param m function: migratory connectivity definded for all breeding areas and over whole wintering area
#' @param r constant: recovery probability, has to be constant over whole wintering area
#' and independent of breeding area
#' @param T integer: length of observation period
#' @return subdensity of recovered individuals for the specified parameters
#' @export
#' @examples f_f_sub()
f_f_sub <- function(w,t,b,s,m,r,T){
  m(b,w)*(s(w)^(t-1)*(1-s(w))*r)
}