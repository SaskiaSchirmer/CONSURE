#' estimate raw density of recovered individuals
#'
#' This function estimates the raw density of recovered individuals in space and time
#' from given data using a kernel density estimate.
#' @param eta list with B entries: data of recovered individuals.
#' eta[[b]][,1] contains the 1-dimensional space of recovery,
#' eta[[b]][,2] contains the integer age at recovery
#' @param B integer, number of breeding areas
#' @param T integer: length of observation period
#' @param res_x resolution in space
#' @param res_y resolution in y-direction. (Irrelevant in 1D setting). Defaults to res_x.
#' @param all boolean: if TRUE only one kernel density estimate will be calculated
#' summarising all breeding areas. Defaults to FALSE.
#' @return list of values created by sparr::spattemp.density (see ?sparr::spattemp.density for details)
#' @export
#' @examples estKDE()
estKDE <- function(eta, B, T, res_x, res_y = res_x, all = FALSE){
  if(all){
    eta <- list(do.call("rbind",eta))
    B <- 1
  }

  kde <- list()
  for(b in 1:B){

    x <- eta[[b]][,1]
    y <- runif(length(eta[[b]][,1]), 0, 1)
    pp <- spatstat::ppp(x,y,c(0,1),c(0,1), marks = eta[[b]][,2])
    kde[[b]] <- sparr::spattemp.density(pp, h = 0.08,
                                 tt = pp$marks,
                                 lambda = 1.1,
                                 tlim = c(1,T),
                                 sedge = "uniform", tedge = "uniform",
                                 sres = res_x)

  }
  kde
}
