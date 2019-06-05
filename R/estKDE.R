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
estKDE <- function(markRecaptureObject, res_x, res_y = res_x, all = FALSE, dataType = "sim"){

  eta <- markRecaptureObject$data[[dataType]]$eta
  B <- markRecaptureObject$numberOfBreedingAreas
  T <- markRecaptureObject$observationTime

  if(all){
    eta <- list(do.call("rbind",eta))
    b <- "all"
    x <- eta[[1]][,"x"]


    y <- try(eta[[1]][,"y"])
    if("try-error" %in% class(y)) y <- runif(length(eta[[1]][,"x"]), 0, 1)



    pp <- spatstat::ppp(x,y,c(0,1),c(0,1), marks = eta[[1]][,"time"])
    markRecaptureObject$kde[[dataType]][[b]] <- sparr::spattemp.density(pp, h = 0.08,
                                                                        tt = pp$marks,
                                                                        lambda = 1.1,
                                                                        tlim = c(1,T),
                                                                        sedge = "uniform", tedge = "uniform",
                                                                        sres = res_x)
  } else{
    for(b in 1:B){

      x <- eta[[b]][,"x"]


      y <- try(eta[[b]][,"y"])
      if("try-error" %in% class(y)) y <- runif(length(eta[[b]][,1]), 0, 1)



      pp <- spatstat::ppp(x,y,c(0,1),c(0,1), marks = eta[[b]][,"time"])
      markRecaptureObject$kde[[dataType]][[paste("b",b,sep = "")]] <- sparr::spattemp.density(pp, h = 0.08,
                                                                          tt = pp$marks,
                                                                          lambda = 1.1,
                                                                          tlim = c(1,T),
                                                                          sedge = "uniform", tedge = "uniform",
                                                                          sres = res_x)

    }
  }

  return(markRecaptureObject)
}
