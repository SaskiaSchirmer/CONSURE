#' function defining the smoothness of a bspline
#'
#' This function defines the smoothness of a bspline as the second derivative
#' @param beta vector of parameters of the b-spline
#' @param dim dimension of space
#' @param A matrix of second derivative of b-spline
#' @param inside specifies if a cell of the gridded window is inside the window of the data
#'               or not. Vector of logicals.
#'
#' @return function defining the distance between a b-spline and
#'         discrete migratory connectivity depending on the parameters
#' @export
#' @examples Lh(w,beta,knots,degree)

Lh <- function(beta, dim,A,normalize){

    func <- function(beta){
        t(beta)%*%A%*%beta/normalize
    }

    return(func)
}
