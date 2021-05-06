#' gradient of function defining the smoothness of a bspline
#'
#' This function defines the smoothness of a bspline as the second derivative
#' @param beta vector of parameters of the b-spline
#' @param k number of parameters
#' @param dim dimension of space
#' @param A matrix of second derivative of b-spline
#' @param normalize numeric, normalizes the discretized integralt. Equals to the spatial
#'  resolution in one-dimensional space and to the product of the spatial resolutions in
#'  two-dimensional space.
#'
#' @return function defining the distance between a b-spline and
#'         discrete migratory connectivity depending on the parameters
#' @export
#' @examples{
#'     y <- seq(0,1,length.out=100)
#'     iK <- seq(0.1111111,0.8888889,length.out=8)
#'     grL <- grLh(beta,k,dim = 1,
#'         A = splines2::dbs(y,knots=iK,derivs = 2, degree = 3, intercept = TRUE),
#'         normalize = 100)
#'     grL(beta = rnorm(12), k = 2)
#' }

grLh <- function(beta,k,dim,A,normalize){

    func <- function(beta,k){
      if(dim == 1){
        A_k <- sum(A%*%beta*A[,k])
        #A_k <- A[,k]
      } else if(dim == 2){

        A_k <- sum(A$A_vv%*%beta*A$A_vv[,k]+
                     2*A$A_vw%*%beta*A$A_vw[,k]+
                     A$A_ww%*%beta*A$A_ww[,k])

        #A_k <- t(A$A_vv[,k])%*%A$A_vv[,k] + t(A$A_ww[,k])%*%A$A_ww[,k] + 2*(t(A$A_vw[,k])%*%A$A_vw[,k])
      } else{
        message("grLh: Check number of dimensions.")
      }

      #t(A_k)%*%A_k#/normalize
      2*A_k/normalize
    }

   return(func)
}
