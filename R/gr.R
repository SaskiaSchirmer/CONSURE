#' gradient of penalizing function for combined parameter estimate
#'
#' This function defines the penalization term for the combined estimation approach.
#' It can be used to jointly optimize distance to continuous migratory connectivity estimates
#' gained by estM, discrete migratory connectivity estimates and maximize smoothness.
#' @param beta vector of parameters of the optimization function
#' @param rawSpline auxiliary spline spanning the whole wintering area
#' @param m vector of continuous migratory connectivity estimates
#' @param lambda weights for different penalization terms
#' @param split vector of length of y which defines the affiliation to a discrete
#'              wintering area
#' @param A matrix of second derivative of smoothing
#' @param prop vector of proportions of individuals going to discrete wintering areas
#'             (discrete estimate or expected value for migratory connectivity)
#' @param dim numeric, spatial dimension
#' @param res numeric, spatial resolution
#' @param inside specifies if a cell of the gridded window is inside the window of the data
#'               or not. Vector of logicals.
#' @param normalize numeric, normalizes the discretized integralt. Equals to the spatial
#'  resolution in one-dimensional space and to the product of the spatial resolutions in
#'  two-dimensional space.
#'
#' @return function depending on bspline parameters, which returns the sum of
#'         quadratic distances to continuous and discrete migratory connectivity
#'         and smoothness
#' @export
#' @examples{
#'     y <- seq(0,1,length.out=100)
#'     iK <- seq(0.1111111,0.8888889,length.out=8)
#'     rS <- initSpline(y=y,
#'         knots = iK,
#'         degree = 3,
#'         intercept = TRUE,
#'         dim = 1)
#'      gr <- gr(beta,
#'          rawSpline = rS,
#'          m = mro1DIncreasing$mro$estimates$m$all,
#'          lambda  = c(.05,300),
#'          split =mro1DIncreasing$split,
#'          A = splines2::dbs(y,knots=iK,derivs = 2, degree = 3, intercept = TRUE),
#'          prop = mro1DIncreasing$mro$breedingAreas$all$mDiscrete/
#'              sum(mro1DIncreasing$mro$breedingAreas$all$mDiscrete),
#'          dim = 1,
#'          res = 100,
#'          inside = rep(1,100),
#'          normalize = 100)
#'      gr(rnorm(12))
#' }

gr <- function(beta,rawSpline,m,lambda, split,
               A, prop, dim, res, inside, normalize){
  print(paste("inGr"))

  if(dim == 1){
    inside <- colSums(inside) > 0
  }

  k <- NULL

  numberOfParameters <- ncol(rawSpline)

  print(paste("gr",dim))
  continuous <- grIntegrateDist2Continuous(beta,k,rawSpline = rawSpline,
                                         dim = dim, m = m,
                                         inside = inside, normalize = sum(inside, na.rm = TRUE))
  print("gr:ConToDisc")
  discrete <- grIntegrateDist2Discrete(beta,k,rawSpline = rawSpline,
                                     dim = dim,
                                     split = split,prop = prop,
                                     inside = inside)
  print("gr:DiscToSmooth")
  smooth <- grLh(beta,k,dim = dim, A = A, normalize = sum(inside, na.rm = TRUE))
  print("gr:outSmooth")

  returnFunc <- function(beta,k){
    lambda[1]*smooth(beta=beta,k=k)+
      lambda[2]*discrete(beta = beta,k=k)+
      continuous(beta = beta,k=k)
  }
  returnFunc <- Vectorize(returnFunc,vectorize.args = "k")

  function(beta){
    return(
      returnFunc(beta, 1:numberOfParameters)
    )
  }
}
