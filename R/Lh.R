#' function defining the smoothness of a bspline
#'
#' This function defines the smoothness of a bspline as the second derivative
#' @param beta vector of parameters of the b-spline
#' @param dim dimension of space
#' @param A matrix of second derivative of b-spline
#' @param normalize factor to normalize with.
#'
#' @return function defining the distance between a b-spline and
#'         discrete migratory connectivity depending on the parameters
#' @export
#' @examples{
#'     y <- seq(0,1,length.out=100)
#'     iK <- seq(0.1111111,0.8888889,length.out=8)
#'      A <- splines2::dbs(y,knots=iK,derivs = 2, degree = 3, intercept = TRUE)
#'     L <- Lh(beta,dim = 1,
#'         A = t(A)%*%A,
#'         normalize = 100)
#'     L(beta = rnorm(12))
#' }
Lh <- function(beta, dim,A,normalize){

    func <- function(beta){
        t(beta)%*%A%*%beta/normalize
    }

    return(func)
}
