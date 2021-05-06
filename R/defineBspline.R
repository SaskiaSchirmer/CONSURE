#' function to define a positive a b-spline function depending on its parameters
#'
#' This function defines the exponential of a b-spline dependent on its parameters beta
#' @param rawSpline raw spline, value of initSpline()
#' @param beta parameter of the b-spline, values are to be optimized
#' @param inside specifies if a cell of the gridded window is inside the window of the data
#'               or not. Vector of logicals.
#'
#' @return function defining a b-spline for a given parameter vector
#' @export
#' @examples{
#'     y <- seq(0,1,length.out=100)
#'     iK <- seq(0.1111111,0.8888889,length.out=8)
#'     rS <- initSpline(y=y,
#'         knots = iK,
#'         degree = 3,
#'         intercept = TRUE,
#'         dim = 1)
#'     dB <- defineBspline(rawSpline = rS,
#'         beta,
#'         inside = rep(TRUE, 100))
#'     dB(beta = rnorm(12))
#' }


defineBspline <- function(rawSpline,beta,inside){
    print("defBspline")

  return(
    function(beta){
      exp(rawSpline%*%beta)*inside
    }
  )
}
