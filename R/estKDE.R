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
#' @param xname name of x variable, e.g. longitude
#' @param yname name of y variable, e.g. latitude
#' @param timename name of time variable, e.g. age
#' @return list of values created by sparr::spattemp.density (see ?sparr::spattemp.density for details)
#' @export
#' @examples estKDE()
estKDE <- function(markRecaptureObject, res_x, res_y = res_x, all = FALSE, dataType = "sim",
                   xname  = "x", yname = "y", timename = "time"){

  eta <- markRecaptureObject$winteringArea[[dataType]]
  B <- markRecaptureObject$numberOfBreedingAreas
  T <- markRecaptureObject$observationTime
  win <- markRecaptureObject$winteringArea$window
  if(identical(win$yrange,c(0,0))) win$yrange <- c(0,1)
  breedingAreaNames <- names(markRecaptureObject$breedingAreas)[!grepl("all",names((markRecaptureObject$breedingAreas)))]



  if(all){
    eta <- list(do.call("rbind",eta))
    x <- eta[[1]][,xname]


    y <- try(eta[[1]][,yname], silent = TRUE)
    if("try-error" %in% class(y)) y <- runif(length(eta[[1]][,xname]), 0, 1)



    pp <- spatstat::ppp(x,y,window = win, marks = eta[[1]][,timename])
    h <- sparr::OS.spattemp(pp)
    markRecaptureObject$kde[[dataType]][["all"]] <- sparr::spattemp.density(pp, h = h[1],
                                                                        tt = pp$marks,
                                                                        lambda = 1.1,
                                                                        tlim = c(1,T),
                                                                        sedge = "uniform", tedge = "uniform",
                                                                        sres = res_x)
  } else{
    for(b in breedingAreaNames){

      x <- eta[[b]][,xname]


      y <- try(eta[[b]][,yname], silent = TRUE)
      if("try-error" %in% class(y)) y <- runif(length(eta[[b]][,1]), 0, 1)



      pp <- spatstat::ppp(x,y,window = win, marks = eta[[b]][,timename])
      h <- sparr::OS.spattemp(pp)
      markRecaptureObject$kde[[dataType]][[b]] <- sparr::spattemp.density(pp, h = h[1],
                                                                          tt = pp$marks,
                                                                          lambda = 1.1,
                                                                          tlim = c(1,T),
                                                                          sedge = "uniform",
                                                                          tedge = "uniform",
                                                                          sres = res_x)

    }
  }

  return(markRecaptureObject)
}
