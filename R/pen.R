#' penalization function for combined parameter estimate
#'
#' This function defines the penalization term for the combined estimation
#' approach. It can be used to jointly optimize distance to continuous migratory
#' connectivity estimates gained by estM, discrete migratory connectivity
#' estimates and maximize smoothness.
#' @inheritParams define_bspline
#' @param m vector of continuous migratory connectivity estimates
#' @param b name of breeding area
#' @param lambda weights for different penalization terms
#' @param split vector of length of y which defines the affiliation to a
#'              discrete wintering area
#' @param A squared second derivative of B-spline
#' @param prop vector of proportions of individuals going to discrete wintering
#'             areas (discrete estimate or expected value for migratory
#'             connectivity)
#' @param dim numeric, spatial dimension
#' @param res numeric, spatial resolution
#' @param normalize numeric, normalizes the discretized integralt. Equals to
#'                  the spatial resolution in one-dimensional space and to the
#'                  product of the spatial resolutions in two-dimensional space.
#'
#' @return function depending on bspline parameters, which returns the sum of
#'         quadratic distances to continuous and discrete migratory connectivity
#'         and smoothness
#' @export
#'
#' @examples{
#'     y <- seq(0,1,length.out=100)
#'     i_k <- seq(0.1111111,0.8888889,length.out=8)
#'     r_s <- init_spline(y = y,
#'         knots = i_k,
#'         degree = 3,
#'         intercept = TRUE,
#'         dim = 1)
#'      A <- splines2::dbs(y, knots = i_k, derivs = 2,
#'                         degree = 3, intercept = TRUE)
#'      penalty <- pen(beta,
#'          raw_spline = r_s,
#'          m = mro1D_increasing$mro$estimates$m$all,
#'          b = "all",
#'          lambda  = c(.05, 300),
#'          split = mro1D_increasing$split,
#'          A = t(A) %*% A,
#'          prop = mro1D_increasing$mro$origins$all$m_discrete/
#'              sum(mro1D_increasing$mro$origins$all$m_discrete),
#'          dim = 1,
#'          res = 100,
#'          inside = rep(1, 100),
#'          normalize = 100)
#'      penalty(rnorm(12))
#' }

pen <- function(beta, raw_spline, m, b, lambda, split, A,
                prop, dim, res, inside, normalize) {
  print(paste("inPen"))

  if (dim == 1) {
    inside <- colSums(inside) > 0
  }
  print(paste("pen", dim))
  continuous <- integrate_dist_continuous(
    raw_spline = raw_spline,
    beta = beta, m = m,
    inside = inside, normalize = sum(inside, na.rm = TRUE)
  )
  print("outCon")
  discrete <- integrate_dist_discrete(
    raw_spline = raw_spline,
    dim = dim, beta = beta, b = b,
    split = split, prop = prop,
    print = FALSE, inside = inside
  )

  smooth <- lh(dim = dim, A = A, normalize = sum(inside, na.rm = TRUE))


  function(beta) {
    if ((lambda[1] * smooth(beta)) == 0) warning("Smooth part of penalty
                                                 function is 0. Please check!")
    if ((lambda[2] * discrete(beta = beta)) == 0) warning("Discrete part of
                                                          penalty function is 0.
                                                          Please check!")
    if ((continuous(beta = beta)) == 0) warning("Continuous part of penalty
                                                function is 0. Please check!")

    return(
      lambda[1] * smooth(beta) +
        lambda[2] * discrete(beta = beta) +
        continuous(beta = beta)
    )
  }
}
