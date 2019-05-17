#' subdensity function for not seen individuals
#'
#' This function sets up the subdensity function for every spatiotemporal point for not seen individuals
#' by means of given survival, migratory connectivity and recovery probability. Not seen individuals died
#' in observation time but were not found or survived observation time.
#' @param w decimal number: spatial point of recovery (1-dimensional)
#' @param b integer: breeding area
#' @param s function: survival function defined over whole wintering area, independent of breeding area
#' @param m function: migratory connectivity definded for all breeding areas and over whole wintering area
#' @param r constant: recovery probability, has to be constant over whole wintering area
#' and independent of breeding area
#' @param T integer: length of observation period
#' @return subdensity of recovered individuals for the specified parameters
#' @export
#' @examples f_nf_sub()
f_nf_sub <- function(w,b,s,m,r,T){
  ((1-r)*(1-s(w)^T)+s(w)^T)*m(b,w)
}
