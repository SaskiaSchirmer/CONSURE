#' function to define the distance between a b-spline representing a density
#' and the continuous migratory connectivity estimate
#'
#' This function numerically integrates the quadratic distance between a b-spline
#' representing a density and the continuous migratory connectivity estimate
#' @inheritParams defineBspline
#' @param dim spatial dimension of the data
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
#' @examples integrateDist2Continuous()

integrateDist2Continuous <- function(rawSpline, dim, beta,m,inside, normalize){
    print(paste("intCon",dim))

   # w <- y$longitude # auf 2 Dim erweitern!!

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
