#' function to define the distance between a b-spline representing a density
#' and the continuous migratory connectivity estimate
#'
#' This function numerically integrates the quadratic distance between a b-spline
#' representing a density and the continuous migratory connectivity estimate
#' @inheritParams defineBspline
#' @param m vector of continuous migratory connectivity estimate
#' @param inside specifies if a cell of the gridded window is inside the window of the data
#'               or not. Vector of logicals.
#' @param normalize numeric, normalizes the discretized integralt. Equals to the spatial
#'  resolution in one-dimensional space and to the product of the spatial resolutions in
#'  two-dimensional space.
#'
#' @return function defining the distance between a b-spline and
#'         continuous migratory connectivity
#' @export
#' @examples{
#'     y <- seq(0,1,length.out=100)
#'     iK <- seq(0.1111111,0.8888889,length.out=8)
#'     rS <- initSpline(y=y,
#'         knots = iK,
#'         degree = 3,
#'         intercept = TRUE,
#'         dim = 1)
#'     iC <- integrateDist2Continuous(rawSpline = rS, dim = 1,
#'     beta, m = mro1DIncreasing$estimates$m$all, inside = rep(TRUE,100),
#'     normalize = 100)
#'     iC(rnorm(12))
#' }

integrateDist2Continuous <- function(rawSpline, beta,m,inside, normalize){
    print(paste("intCon"))



    bspline <- defineBspline(rawSpline = rawSpline, beta = beta, inside = inside)


    if(sum(is.nan(m)) !=0) message("matrix of continuous m contains NaN values")

  function(beta){
    return(
      sum(
        (
          bspline(beta)/
            sum(bspline(beta))*normalize
          -c(m)
        )^2, na.rm = TRUE
      )#/normalize
    )
  }
}
