#' gradient function to define the distance between a b-spline representing a density
#' and the discrete proportion
#'
#' This function numerically integrates the quadratic distance between a b-spline
#' representing a density and the discrete migratory connectivity
#' @inheritParams defineBspline
#' @param dim spatial dimension of the data
#' @param split vector of length of y which defines the affiliation to a discrete
#'              wintering area
#' @param b name of breeding area
#' @param prop vector of proportions of individuals going to discrete wintering areas
#'             (discrete estimate or expected value for migratory connectivity)
#' @param print logical, should proportions be printed or not?
#' @param inside specifies if a cell of the gridded window is inside the window of the data
#'               or not. Vector of logicals.
#'
#' @return function defining the distance between a b-spline and
#'         discrete migratory connectivity depending on the parameters
#' @export
#' @examples integrateDist2Discrete()

grIntegrateDist2Discrete <- function(beta,k,rawSpline,dim,
                                   split,
                                   prop,
                                   inside){
  print("intDisc")


  bspline <- defineBspline(rawSpline = rawSpline, beta =beta, inside = inside)



  return(
    function(beta,k){

      ls_rawSpline <- list()
      ls_bspline <- list()

      for(i in unique(split)){
        ls_rawSpline[[i]] <- rawSpline[split == i,]
        ls_bspline[[i]] <- bspline(beta)[split == i,]

      }

      return(sum(2*(sapply(ls_bspline, function(x) sum(x)/sum(bspline(beta)))-as.numeric(prop))*
                   (sapply(Map('*',lapply(ls_rawSpline,function(x) x[,k]),ls_bspline),sum)*
                      sum(bspline(beta))-
                      sapply(ls_bspline, sum)*
                      sum(rawSpline[,k]*bspline(beta))
                   )/sum(bspline(beta))^2/as.numeric(prop)))
    }
  )

}
