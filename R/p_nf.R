#' overall probability to be not seen
#'
#' This function integrates the subdensity of not seen individuals.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @return list: probability to be not seen independent of space
#' and time for every breeding area
#' @export
#' @examples p_nf()
p_nf <- function(b,markRecaptureObject){
 # B <- markRecaptureObject$numberOfBreedingAreas
  p_nf <- numeric()
  if(identical(markRecaptureObject$winteringArea$window$xrange,c(0,0))){
    lb <- markRecaptureObject$winteringArea$window$yrange[1]
    ub <- markRecaptureObject$winteringArea$window$yrange[2]
  } else if(identical(markRecaptureObject$winteringArea$window$yrange,c(0,0))){
    lb <- markRecaptureObject$winteringArea$window$xrange[1]
    ub <- markRecaptureObject$winteringArea$window$xrange[2]
    } else{
      lb <- c(markRecaptureObject$winteringArea$window$xrange[1],
              markRecaptureObject$winteringArea$window$yrange[1])
      ub <- c(markRecaptureObject$winteringArea$window$xrange[2],
              markRecaptureObject$winteringArea$window$yrange[2])
    }
  #for(b in 1:B){
    p_nf <- cubature::adaptIntegrate(f = f_nf_sub,lower = lb, upper = ub, b = b,
                                        markRecaptureObject = markRecaptureObject)$integral
  #}
  return(p_nf)
}
