#' gradient of penalizing function for combined parameter estimate
#'
#' This function defines the penalization term for the combined estimation approach.
#' It can be used to jointly optimize distance to continuous migratory connectivity estimates
#' gained by estM, discrete migratory connectivity estimates and maximize smoothness.
#' @param beta vector of parameters of the optimization function
#' @param y auxiliary vector spanning the whole wintering area
#' @param knots vector of inner knots for b-spline definition
#' @param m vector of continuous migratory connectivity estimates
#' @param prop vector of proportions of individuals going to discrete wintering areas
#'             (discrete estimate or expected value for migratory connectivity)
#' @param degree degree of bspline
#' @param lambda weights for different penalization terms
#' @param split vector of length of y which defines the affiliation to a discrete
#'              wintering area
#' @param inside specifies if a cell of the gridded window is inside the window of the data
#'               or not. Vector of logicals.
#'
#' @return function depending on bspline parameters, which returns the sum of
#'         quadratic distances to continuous and discrete migratory connectivity
#'         and smoothness
#' @export

gr <- function(beta,rawSpline,m,lambda, split, A, prop, dim, res, inside, normalize){
  print(paste("inGr"))

  numberOfParameters <- ncol(rawSpline)

  print(paste("gr",dim))
  continuous <- grIntegrateDist2Continuous(beta,k,rawSpline = rawSpline,
                                         dim = dim, m = m,
                                         inside = inside, normalize = normalize)
  print("gr:ConToDisc")
  discrete <- grIntegrateDist2Discrete(beta,k,rawSpline = rawSpline,
                                     dim = dim,
                                     split = split,prop = prop,
                                     inside = inside)
  print("gr:DiscToSmooth")
  smooth <- grLh(beta,k,dim = dim, A = A, normalize = normalize)
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
