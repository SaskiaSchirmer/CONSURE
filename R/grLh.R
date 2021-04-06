#' gradient of function defining the smoothness of a bspline
#'
#' This function defines the smoothness of a bspline as the second derivative
#' @param beta vector of parameters of the b-spline
#' @param dim dimension of space
#' @param A matrix of second derivative of b-spline
#' @param res spatial resolution of the estimates
#' @param inside specifies if a cell of the gridded window is inside the window of the data
#'               or not. Vector of logicals.
#'
#' @return function defining the distance between a b-spline and
#'         discrete migratory connectivity depending on the parameters
#' @export
#' @examples Lh(w,beta,knots,degree)

grLh <- function(beta,k,dim,A,res){

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
      2*A_k
    }

   return(func)
}
