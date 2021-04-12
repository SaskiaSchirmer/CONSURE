#' estimate raw density of recovered individuals
#'
#' This function estimates the raw density of recovered individuals in space and time
#' from given data using a kernel density estimate.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param res resolution in space
#' @param all boolean: if TRUE only one kernel density estimate will be calculated
#' summarising all breeding areas. Defaults to FALSE.
#' @param xname name of x variable, e.g. longitude
#' @param yname name of y variable, e.g. latitude
#' @param timename name of time variable, e.g. age
#' @param bw numeric. bandwidth parameter. Defaults to NULL.
#' @return markRecaptureObject with list of values created by sparr::spattemp.density (see ?sparr::spattemp.density for details) and spatial resolution.
#' @export
#' @examples estKDE()
estKDE <- function(markRecaptureObject, all = FALSE,
                   xname  = "longitude", yname = "latitude", timename = "age", bw = NULL){

  eta <- markRecaptureObject$winteringArea$recoveryData
  B <- markRecaptureObject$numberOfBreedingAreas
  T <- markRecaptureObject$observationTime
  win <- markRecaptureObject$winteringArea$window
  if(identical(win$yrange,c(0,0))) win$yrange <- c(0,1)
    breedingAreaNames <- names(markRecaptureObject$breedingAreas)[!grepl("all",names((markRecaptureObject$breedingAreas)))]
   markRecaptureObject$spatialResolution <- res



  if(all){
    eta <- list(do.call("rbind",eta))
    x <- eta[[1]][,xname]


    y <- try(eta[[1]][,yname], silent = TRUE)
    if("try-error" %in% class(y)) y <- runif(length(eta[[1]][,xname]), 0, 1)

    pp <- spatstat.geom::ppp(x,y,window = win, marks = eta[[1]][,timename])
    if(is.null(bw)){h <- sparr::OS(pp)} else{h <- bw}

    markRecaptureObject$kde[["all"]] <- sparr::spattemp.density(pp, h = h[1],
                                                                        tt = pp$marks,
                                                                        lambda = 1.1,
                                                                        tlim = c(1,T),
                                                                        sedge = "uniform", tedge = "uniform",
                                                                        sres = res)

    intAllT <- sum(sapply(markRecaptureObject$kde[["all"]]$z, spatstat.geom::integral))

    for(t in 1:T){
      markRecaptureObject$kde[["all"]]$z[[t]] <- markRecaptureObject$kde[["all"]]$z[[t]]/intAllT
    }

    #intTest <- sum(sapply(tmp, integral))
  } else{
    for(b in breedingAreaNames){

      x <- eta[[b]][,xname]


      y <- try(eta[[b]][,yname], silent = TRUE)
      if("try-error" %in% class(y)) y <- runif(length(eta[[b]][,1]), 0, 1)



      pp <- spatstat.geom::ppp(x,y,window = win, marks = eta[[b]][,timename])
      if(is.null(bw)){h <- sparr::OS(pp)} else{h <- bw}
      print(h)
      markRecaptureObject$kde[[b]] <- sparr::spattemp.density(pp, h = h[1],
                                                                          tt = pp$marks,
                                                                          lambda = 1.1,
                                                                          tlim = c(1,T),
                                                                          sedge = "uniform",
                                                                          tedge = "uniform",
                                                                          sres = res)

      intAllT <- sum(sapply(markRecaptureObject$kde[[b]]$z, spatstat.geom::integral))

      for(t in 1:T){
        markRecaptureObject$kde[[b]]$z[[t]] <- markRecaptureObject$kde[[b]]$z[[t]]/intAllT
      }


    }
  }

  return(markRecaptureObject)
}
