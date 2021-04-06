#' penalization function for combined parameter estimate
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

pen <- function(beta,rawSpline,m,b,lambda, split, A, prop, dim, res, inside,normalize){
    print(paste("inPen"))


    print(paste("pen",dim))
  continuous <- integrateDist2Continuous(rawSpline = rawSpline,
                                         dim = dim, beta = beta, m = m,res = res,
                                         inside = inside, normalize = normalize)
    print("outCon")
  discrete <- integrateDist2Discrete(rawSpline = rawSpline,
                                     dim = dim, beta = beta,b=b,
                                     res = res, split = split,prop = prop,
                                     print = FALSE, inside = inside, normalize = normalize)

  smooth <- Lh(dim = dim, A = A, res = res, normalize = normalize)


  function(beta){
    smoothValue <<- c(smoothValue, lambda[1]*smooth(beta))
    discreteValue <<- c(discreteValue, lambda[2]*discrete(beta = beta))
    continuousValue <<- c(continuousValue, continuous(beta = beta))
    index <<- index+1

    print(paste("smooth",tail(smoothValue,1)))
    print(paste("discrete",tail(discreteValue,1)))
    print(paste("continuous",tail(continuousValue,1)))

    return(
      lambda[1]*smooth(beta) +
        lambda[2]*discrete(beta = beta) +
        continuous(beta = beta)
    )
  }
}
