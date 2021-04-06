#' Function to initialize a b-spline for optimization.
#'
#' This function initilizes a b-spline on a helper sequence to be used for optimization.
#' @param y numeric vector, helper sequence, has to span whole wintering area, defaults to
#' seq(0,1,length.out = 100)
#' @param knots numeric vector, inner knot sequence to span the b-spline
#' @param degree integer, degree of the piecewise polynomial the b-spline is made of,
#' defaults to 3 (cubic b-spline)
#' @param intercept logical, defines if intercept is estimated or not, defaults to TRUE,
#' do not change default without good reason!
#' @param dim spatial dimension. Integer. Can be 1 or 2.
#' @return returns a bSpline. See ?splines2::bSpline for details.
#' @export
#'
#' @examples initSpline()

initSpline <- function(y, knots,degree, intercept = TRUE, dim){
    print("startInitSpline")
    print(knots)
    if(dim == 1){
       # if(sum(sapply(y,is.null)) == 1){
        #    y <- unname(unlist(y))
        #} else{ message("not sure how to use 'y'")}

        #if(sum(sapply(knots,is.null)) == 1){
         #   knots <- unname(unlist(knots))
        #} else{ message("not sure how to use 'knots'")}
        #print(paste("initKnots",knots))
        bspline <- splines2::bSpline(y,knots = knots, degree = degree, intercept = intercept)
    }else if(dim == 2){
        bspline <- splines2::bSpline(y$longitude,knots=knots$longitude,degree = degree, intercept = TRUE)%x%
  splines2::bSpline(y$latitude,knots=knots$latitude,degree = degree, intercept = TRUE)
    }
        return(bspline)
    }
