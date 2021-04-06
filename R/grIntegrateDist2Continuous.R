#' gradient of function to define the distance between a b-spline representing a density
#' and the continuous migratory connectivity estimate
#'
#' This function numerically integrates the quadratic distance between a b-spline
#' representing a density and the continuous migratory connectivity estimate
#' @inheritParams defineBspline
#' @param dim spatial dimension of the data
#' @param m vector of continuous migratory connectivity estimate
#' @param res spatial resolution of estimates
#' @param inside specifies if a cell of the gridded window is inside the window of the data
#'               or not. Vector of logicals.
#'
#' @return function defining the distance between a b-spline and
#'         continuous migratory connectivity
#' @export
#' @examples grIntegrateDist2Continuous()

grIntegrateDist2Continuous <- function(beta,k,rawSpline, dim,m,res, inside,normalize){
  print(paste("intCon",dim))


  bspline <- defineBspline(rawSpline = rawSpline, beta = beta, inside = inside)

  dh <- function(beta,k){
    dhsum <- numeric()
    b <- bspline(beta)
    for(j in 1:normalize){

      dhsum[j] <- sum((rawSpline[j,k] - rawSpline[,k])*b)
    }

    return(dhsum)
  }

  if(sum(is.nan(m)) !=0) message("matrix of continuous m contains NaN values")



  function(beta,k){
    return(
      2/sum(bspline(beta))^2*
      sum(
        (
          bspline(beta)/sum(bspline(beta))*normalize
          -c(m)
        )*bspline(beta)*dh(beta,k), na.rm = TRUE
      )
    )
  }
}
