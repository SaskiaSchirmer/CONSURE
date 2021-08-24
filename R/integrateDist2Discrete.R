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
#'
#' @return function defining the distance between a b-spline and
#'         discrete migratory connectivity depending on the parameters
#' @export
#' @examples{
#'     y <- seq(0,1,length.out=100)
#'     iK <- seq(0.1111111,0.8888889,length.out=8)
#'     rS <- initSpline(y=y,
#'         knots = iK,
#'         degree = 3,
#'         intercept = TRUE,
#'         dim = 1)
#'     iD <- integrateDist2Discrete(rawSpline = rS, dim = 1,
#'         split = mro1DIncreasing$split, beta, b = "all",
#'         prop = mro1DIncreasing$mro$breedingAreas$all$mDiscrete/
#'             sum(mro1DIncreasing$mro$breedingAreas$all$mDiscrete),
#'         inside = rep(TRUE,100))
#'     iD(rnorm(12))
#' }

integrateDist2Discrete <- function(rawSpline,dim,
                                   split,beta,
                                   b,prop,print = TRUE,
                                   inside){
    print("intDisc")

    bspline <- defineBspline(rawSpline = rawSpline, beta =beta, inside = inside)

  return(
    function(beta){
      bspline <- bspline(beta)

      tmp <- as.data.frame(cbind(bspline/sum(bspline),split))
      colnames(tmp) <- c("bspline","split")

      tmp2 <- dplyr::group_by(tmp, split)
      tmp2 <- dplyr::summarise(tmp2, sum = sum(bspline))
      tmp2 <- tmp2[!is.na(tmp2$split),]

      if(print) print(tmp2)

      if(sum(is.infinite(bspline))>0){
        return(Inf)
      }else{

        return(sum(((tmp2$sum - prop)/prop)^2))}
    }
  )

}
