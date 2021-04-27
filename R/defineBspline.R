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
#' @examples defineBspline(w,beta,knots)


defineBspline <- function(rawSpline,beta,inside){
    print("defBspline")

  return(
    function(beta){
      exp(rawSpline%*%beta)*inside
    }
  )
}
