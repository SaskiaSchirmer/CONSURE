#' density function for recovered individuals only
#'
#' This function sets up a valid density function for every spatiotemporal point for recovered individuals
#' by means of given survival, migratory connectivity and recovery probability.
#' @param w decimal number: spatial point of recovery (1-dimensional)
#' @param t integer: temporal component of recovery
#' @param b integer: breeding area
#' @param s function: survival function defined over whole wintering area, independent of breeding area
#' @param m function: migratory connectivity definded for all breeding areas and over whole wintering area
#' @param r constant: recovery probability, has to be constant over whole wintering area
#' and independent of breeding area
#' @param T integer: length of observation period
#' @param f_f function: subdensity of recovered individuals
#' @param f_nf function: subdensity of not recovered individuals
#' @param lb decimal: lower bound of wintering area
#' @param ub decimal: upper bound of wintering area
#' @return density of recovered individuals for the specified parameters
#' @export
#' @examples f_f()
# PhD Saskia Schirmer
# 15.05.2019
# continuous model
# density function for recovered individuals only (normalized subdensity)
f_f <- function(w,t,b,lb,ub,s,m,r,T,f_f_sub, f_nf_sub){
  f_f_sub(w,t,b,s,m,r,T)/(1-integrate(f_nf_sub,lb,ub, b = b,s= s, m=f, r=r, T = T)$value)
}
