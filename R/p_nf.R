#' overall probability to be not seen
#'
#' This function integrates the subdensity of not seen individuals.
#' @param lb lower bound of wintering area (1D). Defaults to 0.
#' @param ub upper bound of wintering area (1D). Defaults to 1.
#' @param s function: survival function defined over whole wintering area, independent of breeding area
#' @param m function: migratory connectivity definded for all breeding areas and over whole wintering area
#' @param r constant: recovery probability, has to be constant over whole wintering area
#' and independent of breeding area
#' @param B integer, number of breeding areas
#' @param T integer: length of observation period
#' @return list of length B: probability to be not seen independent of space and time for every breeding area
#' @export
#' @examplesp_nf()
p_nf <- function(lb = 0,ub = 1,s,m,r,B,T){
  p_nf <- numeric()

  for(b in 1:B){
    p_nf[b] <- integrate(f = f_nf_sub,lower = lb, upper = ub, b = b, s= s, m=m, r=r, T = T)$value
  }
  return(p_nf)
}
