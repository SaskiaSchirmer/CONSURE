#' gradient function to define the distance between a b-spline representing a density
#' and the discrete proportion
#'
#' This function numerically integrates the quadratic distance between a b-spline
#' representing a density and the discrete migratory connectivity
#' @inheritParams defineBspline
#' @param k number of parameters
#' @param dim spatial dimension of the data
#' @param split vector of length of y which defines the affiliation to a discrete
#'              wintering area
#' @param prop vector of proportions of individuals going to discrete wintering areas
#'             (discrete estimate or expected value for migratory connectivity)
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
#'     grID <- grIntegrateDist2Discrete(beta,k,
#'         rawSpline = rS,
#'         dim = 1,
#'         split = mro1DIncreasing$split,
#'         prop = mro1DIncreasing$mro$breedingAreas$all$mDiscrete/
#'              sum(mro1DIncreasing$mro$breedingAreas$all$mDiscrete),
#'         inside = rep(1,100))
#'    grID(beta = rnorm(12), k = 2)
#' }

grIntegrateDist2Discrete <- function(beta,k,rawSpline,dim,
                                   split,
                                   prop,
                                   inside){
  print("intDisc")


  bspline <- defineBspline(rawSpline = rawSpline, beta =beta, inside = inside)



  return(
    function(beta,k){

      ls_rawSpline <- list()
      ls_bspline <- list()

      for(i in unique(split[!is.na(split)])){
        ls_rawSpline[[i]] <- rawSpline[!is.na(split) & split == i,]
        ls_bspline[[i]] <- bspline(beta)[!is.na(split) & split == i,]

      }

      return(sum(2*(sapply(ls_bspline, function(x) sum(x)/sum(bspline(beta)))-as.numeric(prop))*
                   (sapply(Map('*',lapply(ls_rawSpline,function(x) x[,k]),ls_bspline),sum)*
                      sum(bspline(beta))-
                      sapply(ls_bspline, sum)*
                      sum(rawSpline[,k]*bspline(beta))
                   )/sum(bspline(beta))^2/as.numeric(prop)))
    }
  )

}
