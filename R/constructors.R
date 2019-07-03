#' constructor for wintering area
#'
#' This function defines the properties of the wintering area.
#' @param window object of class "owin":observation window in two-dimensional plane
#' @param survival function: survival function defined over whole wintering area, independent of breeding area
#' @param recovery constant function: recovery probability, has to be constant over whole wintering area
#' @return object of class "winteringArea": contains list of window, survival and recovery for the wintering area
#' @examples new_winteringArea()

new_winteringArea <- function(window = owin(),
                                survival,
                                recovery){
  stopifnot(spatstat::is.owin(window))
  stopifnot(is.null(survival) | is.function(survival))
  stopifnot(is.null(recovery) | is.function(recovery))
  structure(list(window = window,survival = survival, recovery = recovery), class = "winteringArea")
}

#' helper function for wintering area
#'
#' This function defines the properties of the wintering area using the constructor.
#' @param window object of class "owin":observation window in two-dimensional plane. Can be replaced by using xrange and yrange.
#' @param survival function: survival function defined over whole wintering area, independent of breeding area
#' @param recovery constant function: recovery probability, has to be constant over whole wintering area
#' @param xrange vector in the form of c(xmin,xmax). To define line or rectangle xrange and yrange can be used instead of window.
#' @param yrange vector in the form of c(ymin,ymax). To define line or rectangle xrange and yrange can be used instead of window.
#' @return object of class "winteringArea": contains list of window, survival and recovery for the wintering area
#' @export
#' @examples winteringArea()
winteringArea <- function(window=NULL,survival,recovery,xrange = c(0,0),yrange = c(0,0)){
  try(if(is.null(window) & identical(xrange, c(0,0)) & identical(yrange, c(0,0))){
    stop("Please define either window or x- and/or y-range of wintering area")}else{
      if(!spatstat::is.owin(window)){window <- spatstat::as.owin(list(xrange = xrange, yrange = yrange))}
      return(new_winteringArea(window,survival,recovery))
    })


}

#' constructor for breeding area
#'
#' This function defines the properties of the breeding area.
#' @param markedInds integer: number of marked individuals in this breeding area
#' @param migratoryConnectivity function: migratory connectivity function conditioned
#' on this breeding area defined over whole wintering area
#' @return object of class "breedingArea": contains list of number of marked individuals
#' and migratory connectivity function
#' @examples new_breedingArea()
new_breedingArea <- function(markedInds = numeric(),
                             migratoryConnectivity){
  stopifnot(markedInds%%1==0)
  stopifnot(is.null(migratoryConnectivity)  | is.function(migratoryConnectivity))
  structure(list(markedInds = markedInds, migratoryConnectivity = migratoryConnectivity),
            class = "breedingArea")
}

#' helper function for breeding area
#'
#' This function defines the properties of the breeding area using the constructor.
#' @param markedInds integer: number of marked individuals in this breeding area
#' @param migratoryConnectivity function: migratory connectivity function conditioned
#' on this breeding area defined over whole wintering area
#' @return object of class "breedingArea": contains list of number of marked individuals
#' and migratory connectivity function
#' @export
#' @examples breedingArea()
breedingArea <- function(markedInds,migratoryConnectivity){
  new_breedingArea(markedInds,migratoryConnectivity)
}

#' constructor for mark recapture object
#'
#' This function defines the properties of the mark recapture object.
#' @param winteringArea object of class "winteringArea"
#' @param breedingAreas list of objects of class "breedingAreas"
#' @param yrange vector in the form of c(ymin,ymax). To define line or rectangle xrange and yrange can be used instead of window.
#' @param survival function: survival function defined over whole wintering area, independent of breeding area
#' @param recovery constant function: recovery probability, has to be constant over whole wintering area
#' @param markedInds integer: number of marked individuals in this breeding area
#' @param migratoryConnectivity: either list of functions containing one migratory connectivity
#'  function for every breeding area or function with parameter b, allowing to partialise function
#'  for every breeding area with purrr::partial
#' @param observationTime length of observation window in years
#' @return object of class "markRecaptureObject": contains list of wintering area, breeding areas,
#' observationTime and number of breeding areas
#' @examples new_markRecaptureObject()
#'
new_markRecaptureObject <- function(winteringArea, breedingAreas,observationTime, numberOfBreedingAreas,
                                    spatialDim){

  stopifnot(class(winteringArea) == "winteringArea")
  stopifnot(is.list(breedingAreas))
  stopifnot(observationTime %%1 == 0)
  stopifnot(length(numberOfBreedingAreas)==1)
  stopifnot(numberOfBreedingAreas%%1 == 0)

  structure(list(winteringArea = winteringArea,
                 breedingAreas = breedingAreas,
                 observationTime = observationTime,
                 numberOfBreedingAreas = numberOfBreedingAreas,
                 spatialDim = spatialDim,
                 kde = list(sim = list(), real = list()),
                 data = list(sim = list(), real = list()),
                 estimates = list()), class = "markRecaptureObject")

}

#' helper function for mark recapture object
#'
#' This function defines the properties of the mark recapture object using the constructor.
#' @param window object of class "owin":observation window in two-dimensional plane. Can be replaced by using xrange and yrange.
#' @param xrange vector in the form of c(xmin,xmax). To define line or rectangle xrange and yrange can be used instead of window.
#' @param yrange vector in the form of c(ymin,ymax). To define line or rectangle xrange and yrange can be used instead of window.
#' @param survival function: survival function defined over whole wintering area, independent of breeding area
#' @param recovery constant function: recovery probability, has to be constant over whole wintering area
#' @param markedInds integer: number of marked individuals in this breeding area
#' @param migratoryConnectivity: either list of functions containing one migratory connectivity
#'  function for every breeding area or function with parameter b, allowing to partialise function
#'  for every breeding area with purrr::partial
#' @param observationTime length of observation window in years
#' @return object of class "markRecaptureObject": contains list of wintering area, breeding areas,
#' observationTime and number of breeding areas
#' @export
#' @examples markRecaptureObject()
#'
markRecaptureObject <- function(window = NULL, xrange = c(0,0), yrange = c(0,0),
                    survival = NULL,
                    recovery = NULL,
                    markedInds = NULL,
                    migratoryConnectivity = NULL,
                    observationTime,
                    recoveries = NULL
                    ){
  numberOfBreedingAreas <- length(markedInds)
  winteringArea <- winteringArea(window,survival,recovery,xrange,yrange)
  breedingAreas <- list()

  if(!is.null(migratoryConnectivity)){
    if(is.list(migratoryConnectivity)){
      for(b in 1:numberOfBreedingAreas){
        breedingAreas[[paste("b",b,sep="")]] <- breedingArea(markedInds = markedInds[b],migratoryConnectivity[[b]])
      }
      breedingAreas[["all"]] <- breedingArea(markedInds = sum(markedInds[b]),
                                           Vectorize(function(w){
                                             tmp <- numeric()
                                               for(b in 1:numberOfBreedingAreas){
                                                 tmp[b] <- markedInds[b]*migratoryConnectivity[[b]](w)
                                               }
                                            sum(tmp)/sum(markedInds)
                                           }))
    } else{
      tmpMig <- list()
      for(b in 1:numberOfBreedingAreas){
        tmpMig[[b]] <- functional::Curry(migratoryConnectivity,b=b)
        breedingAreas[[paste("b",b,sep="")]] <- breedingArea(markedInds = markedInds[b],tmpMig[[b]])
      }

      breedingAreas[["all"]] <- breedingArea(markedInds = sum(markedInds),
                                           Vectorize(function(w){
                                             tmp <- numeric()
                                             for(b in 1:numberOfBreedingAreas){
                                               tmp[b] <- markedInds[b]*tmpMig[[b]](w)
                                             }
                                             sum(tmp)/sum(markedInds)
                                           }))
    }
  }

  spatialDim <- 2
  if(identical(yrange,c(0,0))) spatialDim <- 1

  new_markRecaptureObject(winteringArea = winteringArea,
                          breedingAreas = breedingAreas,
                          observationTime = observationTime,
                          numberOfBreedingAreas = numberOfBreedingAreas,
                          spatialDim = spatialDim)
}
