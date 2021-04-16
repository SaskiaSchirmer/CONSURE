#' function to define the distance between a b-spline representing a density
#' and the discrete proportion
#'
#' This function numerically integrates the quadratic distance between a b-spline
#' representing a density and the discrete migratory connectivity
#' @inheritParams defineBspline
#' @param dim spatial dimension of the data
#' @param split vector of length of y which defines the affiliation to a discrete
#'              wintering area
#' @param b name of breeding area
#' @param prop vector of proportions of individuals going to discrete wintering areas
#'             (discrete estimate or expected value for migratory connectivity)
#' @param print logical, should proportions be printed or not?
#' @param inside specifies if a cell of the gridded window is inside the window of the data
#'               or not. Vector of logicals.
#' @param normalize numeric, normalizes the discretized integralt. Equals to the spatial
#'  resolution in one-dimensional space and to the product of the spatial resolutions in
#'  two-dimensional space.
#'
#' @return function defining the distance between a b-spline and
#'         discrete migratory connectivity depending on the parameters
#' @export
#' @examples integrateDist2Discrete()

integrateDist2Discrete <- function(rawSpline,dim,
                                   split,beta,
                                   b,prop,print = TRUE,
                                   inside, normalize){
    print("intDisc")

    bspline <- defineBspline(rawSpline = rawSpline, beta =beta, inside = inside)
print(paste("head Bspline in disc", head(bspline)))

print(paste("prop in Disc",prop))
  return(
    function(beta){
      bspline <- bspline(beta)

      tmp <- as.data.frame(cbind(bspline/sum(bspline),split))
      colnames(tmp) <- c("bspline","split")

      tmp2 <- dplyr::group_by(tmp, split)
      tmp2 <- dplyr::summarise(tmp2, sum = sum(bspline))

      if(print) print(tmp2)

      if(sum(is.infinite(bspline))>0){
        return(Inf)
      }else{

        return(sum(((tmp2$sum*normalize - prop)/prop)^2))}
    }
  )

}
