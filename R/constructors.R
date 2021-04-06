#' constructor for wintering area
#'
#' This function defines the properties of the wintering area.
#' @param window object of class "owin":observation window in
#'               two-dimensional plane
#' @param survival function: survival function defined over whole
#'                 wintering area, independent of breeding area
#' @param recovery constant function: recovery probability, must be
#'                 constant over whole wintering area
#' @param sim logical. TRUE if dataset is simulated data containing true functions for
#'            parameters. FALSE if no true parameters are available. UNDER CONSTRUCTION.
#' @param data logical. TRUE if dataset is real-world data. UNDER CONSTRUCTION.
#' @return object of class "winteringArea": contains list of window, survival and recovery for the wintering area
#' @examples new_winteringArea()

new_winteringArea <- function(window = owin(),
                                survival,
                                recovery,
                              sim,
                              data){
  stopifnot(spatstat.geom::is.owin(window))
  stopifnot(is.null(survival) | is.function(survival))
  stopifnot(is.null(recovery) | is.function(recovery))
  structure(list(window = window,survival = survival, recovery = recovery, sim = sim, data = data), class = "winteringArea")
}

#' helper function for wintering area
#'
#' This function defines the properties of the wintering area using the constructor.
#' @inheritParams new_winteringArea
#' @param xrange vector in the form of c(xmin,xmax). To define line or rectangle xrange and yrange can be used instead of window.
#' @param yrange vector in the form of c(ymin,ymax). To define line or rectangle xrange and yrange can be used instead of window.
#' @return object of class "winteringArea": contains list of window, survival and recovery for the wintering area
#' @export
#' @examples winteringArea()
winteringArea <- function(window=NULL,survival,recovery,xrange = c(0,0),yrange = c(0,0),
                          sim = NULL, data = NULL){
  try(if(is.null(window) & identical(xrange, c(0,0)) & identical(yrange, c(0,0))){
    stop("Please define either window or x- and/or y-range of wintering area")}else{
      if(!spatstat.geom::is.owin(window)){window <- spatstat.geom::as.owin(list(xrange = xrange, yrange = yrange))}
      return(new_winteringArea(window,survival,recovery,sim,data))
    })


}

#' constructor for breeding area
#'
#' This function defines the properties of the breeding area.
#' @param markedInds integer: number of marked individuals in this breeding area
#' @param numberOfRecoveries numeric vector. Number of recoveries belonging to each breeding area.
#' @param migratoryConnectivity function: migratory connectivity function conditioned
#' on this breeding area defined over whole wintering area
#' @param sim logical. TRUE if dataset is simulated data containing true functions for
#'            parameters. FALSE if no true parameters are available. UNDER CONSTRUCTION.
#' @param data logical. TRUE if dataset is real-world data. UNDER CONSTRUCTION.
#' @return object of class "breedingArea": contains list of number of marked individuals
#' and migratory connectivity function
#' @examples new_breedingArea()
new_breedingArea <- function(markedInds = numeric(),
                             numberOfRecoveries,
                             migratoryConnectivity,
                             sim,
                             data){
  #stopifnot(markedInds%%1==0)
  stopifnot(is.null(migratoryConnectivity)  | is.function(migratoryConnectivity))
  structure(list(markedInds = markedInds, numberOfRecoveries = numberOfRecoveries,
                 migratoryConnectivity = migratoryConnectivity,
                 sim = sim,
                 data = data),
            class = "breedingArea")
}

#' helper function for breeding area
#'
#' This function defines the properties of the breeding area using the constructor.
#' @inheritParams new_breedingArea
#' @return object of class "breedingArea": contains list of number of marked individuals
#' and migratory connectivity function
#' @export
#' @examples breedingArea()
breedingArea <- function(markedInds,numberOfRecoveries,migratoryConnectivity,
                         sim = NULL, data = NULL){
  new_breedingArea(markedInds,numberOfRecoveries,migratoryConnectivity, sim, data)
}

#' constructor for mark recapture object
#'
#' This function defines the properties of the mark recapture object.
#' @param winteringArea object of class "winteringArea"
#' @param breedingAreas list of objects of class "breedingAreas"
#' @param observationTime single integer. length of observation window in years
#' @param numberOfBreedingAreas single integer. number of breeding areas.
#' @param spatialDim single integer. spatial dimensions, should only be 1 or 2.
#' @return object of class "markRecaptureObject": contains list of wintering area,
#' breeding areas, observationTime, number of breeding areas, spatial dimensions,
#' empty slots for the spatial resolution, the kernel density estimate and the estimates,
#' the class of this object is "markRecaptureObject"
#' @examples new_markRecaptureObject()
#'
new_markRecaptureObject <- function(winteringArea, breedingAreas, observationTime,
                                    numberOfBreedingAreas, spatialDim){

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
                 spatialResolution = NULL,
                 kde = list(sim = list(), data = list()),
                 estimates = list()), class = "markRecaptureObject")

}

#' helper function for mark recapture object
#'
#' This function defines the properties of the mark recapture object using the constructor.
#' @inheritParams winteringArea
#' @param markedInds integer: number of marked individuals in this breeding area
#' @param migratoryConnectivity either list of functions containing one migratory connectivity
#'  function for every breeding area or function with parameter b, allowing to partialise function
#'  for every breeding area with purrr::partial
#' @param observationTime length of observation window in years
#' @return object of class "markRecaptureObject": contains list of wintering area, breeding areas,
#' observationTime, number of breeding areas and spatial dimension
#' @export
#' @examples markRecaptureObject()
#'
markRecaptureObject <- function(window = NULL, xrange = c(0,0), yrange = c(0,0),
                    survival = NULL,
                    recovery = NULL,
                    markedInds,
                    migratoryConnectivity = NULL,
                    observationTime,
                    realRecoveries = NULL,
                    breedingAreaNames = NULL
                    ){
  numberOfBreedingAreas <- length(markedInds)

  if(is.data.frame(realRecoveries)){
    tmp <- list()
    for(area in levels(realRecoveries$markArea)){
      tmp[[area]] <- realRecoveries[realRecoveries$markArea == area,]
    }
  } else{
    tmp <- realRecoveries
  }


  winteringArea <- winteringArea(window,survival,recovery,xrange,yrange,data = tmp)
  breedingAreas <- list()

  if(!is.null(realRecoveries)){numberOfRecoveries <- recIndsFunc(breedingAreaNames,realRecoveries)}

  if(is.null(breedingAreaNames)) breedingAreaNames <- paste("b", 1:numberOfBreedingAreas, sep = "")

  if(!is.null(migratoryConnectivity)){
    if(is.list(migratoryConnectivity)){
      for(b in 1:numberOfBreedingAreas){
        breedingAreas[[breedingAreaNames[b]]] <- breedingArea(markedInds = markedInds[b],
                                                             numberOfRecoveries = NULL,
                                                            migratoryConnectivity =  migratoryConnectivity[[b]],
                                                             data = realRecoveries[realRecoveries$markArea %in% breedingAreaNames[b],])
      }
      breedingAreas[["all"]] <- breedingArea(markedInds = sum(markedInds[b]),
                                             numberOfRecoveries = NULL,
                                          migratoryConnectivity = function(w){
                                             tmp <- matrix(NA, ncol = length(w),nrow = numberOfBreedingAreas)
                                               for(b in 1:numberOfBreedingAreas){
                                                 tmp[b,] <- markedInds[b]*migratoryConnectivity[[b]](w)
                                               }
                                            colSums(tmp)/sum(markedInds)
                                           },
                                           data = realRecoveries)
    } else{
      tmpMig <- list()
      for(b in 1:numberOfBreedingAreas){
        tmpMig[[b]] <- functional::Curry(migratoryConnectivity,b=b,B=numberOfBreedingAreas)
        breedingAreas[[breedingAreaNames[b]]] <- breedingArea(markedInds = markedInds[b],
                                                             numberOfRecoveries = NULL,
                                                             migratoryConnectivity = tmpMig[[b]],
                                                             data = realRecoveries[realRecoveries$markArea %in% breedingAreaNames[b],])
      }

      breedingAreas[["all"]] <- breedingArea(markedInds = sum(markedInds),
                                             numberOfRecoveries = NULL,
                                          migratoryConnectivity = function(w){
                                              tmp <- numeric()
                                             for(b in 1:numberOfBreedingAreas){
                                               tmp[b] <- markedInds[b]*tmpMig[[b]](w)
                                             }
                                             sum(tmp)/sum(markedInds)
                                           },
                                           data = realRecoveries)
    }
  } else{
    for(b in 1:numberOfBreedingAreas){
      breedingAreas[[breedingAreaNames[b]]] <- breedingArea(markedInds = markedInds[b],
                                                           numberOfRecoveries = numberOfRecoveries[b],
                                                           migratoryConnectivity = migratoryConnectivity,
                                                           data = realRecoveries[realRecoveries$markArea %in% breedingAreaNames[b],])
    }

    breedingAreas[["all"]] <- breedingArea(markedInds = sum(markedInds),
                                           numberOfRecoveries = sum(numberOfRecoveries),
                                           migratoryConnectivity = migratoryConnectivity,
                                           data = realRecoveries)

  }

  spatialDim <- 2
  if(is.null(window) & identical(yrange,c(0,0))) spatialDim <- 1

  new_markRecaptureObject(winteringArea = winteringArea,
                          breedingAreas = breedingAreas,
                          observationTime = observationTime,
                          numberOfBreedingAreas = numberOfBreedingAreas,
                          spatialDim = spatialDim)
}

#' constructor for optimization object
#'
#' This function defines the properties of an optimization object.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param initBeta initial values for the B-spline parameters
#' @param y helper sequence
#' @param knots list of latitude and longitude of knots values for B-spline definition
#' @param b character, name of breeding area to be optimized, for all breeding areas use "all"
#' @param degree degree of B-spline
#' @param lambda numeric vector of 2, weights for the discrete and the smoothness constraint
#' @param split split of the discrete non-breeding areas
#' @param penalize function defining penalization term for optimization
#' @param rawSpline spline initialized over helper sequence
#' @param optBeta space for the result of optimization
#' @param values space for the values of the optimization
#' @return object of class "optimizationObject": contains list of all the parameters
#' @examples new_optimizationObject()

new_optimizationObject <- function(markRecaptureObject,
                                   initBeta,
                                   y,
                                   knots,
                                   b,
                                   degree,
                                   lambda,
                                   split,
                                   penalize,
                                   gradient,
                                rawSpline,
                                optBeta = NULL,
                                values = NULL,inside){

  structure(list(markRecaptureObject = markRecaptureObject,
                                   initBeta = initBeta,
                                   y=y,
                                   knots = knots,
                                   b = b,
                                   degree = degree,
                                   lambda = lambda,
                 split = split,
                 penalize = penalize,
                 gradient = gradient,
                 rawSpline = rawSpline,
                 optBeta = optBeta,
                 values = values,
                 inside = inside), class = "optimizationObject")
}

#' helper function for optimization object
#'
#' This function defines the properties of an optimization object.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param initBeta initial values for the B-spline parameters
#' @param y helper sequence, list of longitude and latitude, latitude defaults to NULL
#' @param knots list of latitude and longitude of knots values for B-spline definition,
#' latitude defaults to NULL
#' @param b character, name of breeding area to be optimized, for all breeding areas use "all"
#' @param degree degree of B-spline
#' @param lambda numeric vector of 2, weights for the discrete and the smoothness constraint
#' @param split split of the discrete non-breeding areas
#' @param useCorrectedM logical, defaults to FALSE. Specifies, if the penalizing function
#'  is calculated with the m already corrected for survival and recovery probability. Commonly used for estimating combined migratory connectivity for each
#' breeding area.
#' @param prop defaults to NULL. Proportions for migratory connectivity in discrete space
#'  defined by split.
#' Must be specified if they cannot be calculated from the true continuous migratory connectivity
#' function.
#' @return object of class "optimizationObject": contains list of penalization function,
#' rawSpline and optBeta.
#' @examples new_optimizationObject()
#' @export

optimizationObject <- function(markRecaptureObject, initBeta = NULL,
                               y = list(longitude = seq(0,1,length.out=100),
                                        latitude = NULL),
                               knots = list(longitude = seq(0,1,length.out=10),
                                            latitude = NULL),
                               b, degree = 3,
                               lambda= c(0.0001,10), split,
                               useCorrectedM = FALSE,
                               prop = NULL){

    dim <- markRecaptureObject$spatialDim
    print(paste("dim",dim))

    innerKnots <- list(longitude = knots$longitude[2:(length(knots$longitude)-1)],
                       latitude = knots$latitude[2:(length(knots$latitude)-1)])

     numberOfInnerKnots <- list(
        longitude = length(innerKnots$longitude),
        latitude = length(innerKnots$latitude)
     )

    print(paste("num",numberOfInnerKnots))

    if(is.null(prop)){
      if(!is.null(markRecaptureObject$breedingAreas[[b]]$mDiscrete)){
        prop <- markRecaptureObject$breedingAreas[[b]]$mDiscrete
      }else{
        message("Either define the discrete proportions using prop or calculate them from the true continuous distribution using calcDiscreteM()")
      }

    }else{
      prop <- prop
    }

    dim <- markRecaptureObject$spatialDim
    res <- markRecaptureObject$spatialResolution

    if(useCorrectedM){
      m <- markRecaptureObject$estimates[["mCorrected"]][[b]]
    } else {
      m <- markRecaptureObject$estimates[["m"]][[b]]
    }

    #gridWindow <- expand.grid(seq(markRecaptureObject$winteringArea$window$yrange[1],
     #                             markRecaptureObject$winteringArea$window$yrange[2],length.out = res),
      #                        seq(markRecaptureObject$winteringArea$window$xrange[1],
       #                           markRecaptureObject$winteringArea$window$xrange[2],length.out = res))
    #inside <- spatstat.geom::inside.owin(gridWindow[,2],gridWindow[,1],w=markRecaptureObject$winteringArea$window)
    inside <- c(!is.na(m))

    if(dim == 1){
        if(is.null(initBeta)){
          initBeta <- function(){ rnorm(max(unlist(numberOfInnerKnots))+degree+1)}
        } else {
          tmp <- initBeta
          initBeta <- function(){ tmp}
        }

        if(sum(sapply(y,is.null)) == 1){
            y <- unname(unlist(y))
        } else{ message("not sure how to use 'y'")}

        if(sum(sapply(knots,is.null)) == 1){
           innerKnots <- unname(unlist(innerKnots))
        } else{ message("not sure how to use 'knots'")}

        A <- splines2::dbs(y,knots=innerKnots,derivs = 2, degree = degree, intercept = TRUE)
        A_sqrt <- t(A)%*%A

    } else if(dim == 2){
        if(is.null(initBeta)){
            initBeta <- function(){rnorm((numberOfInnerKnots$longitude+degree+1)*(numberOfInnerKnots$longitude+degree+1))}
        } else{
          tmp <- initBeta
          initBeta <- function(){ tmp}

        }

        A_vv <- splines2::dbs(y$longitude,knots=innerKnots$longitude,derivs = 2,degree = degree, intercept = TRUE)%x%
            splines2::bSpline(y$latitude,knots=innerKnots$latitude,degree = degree, intercept = TRUE)
        A_ww <- splines2::bSpline(y$longitude,knots=innerKnots$longitude,degree = degree, intercept = TRUE)%x%
            splines2::dbs(y$latitude,knots=innerKnots$latitude,derivs = 2,degree = degree, intercept = TRUE)
        A_vw <- splines2::dbs(y$longitude,knots=innerKnots$longitude,derivs = 1,degree = degree, intercept = TRUE)%x%
            splines2::dbs(y$latitude,knots=innerKnots$latitude,derivs = 1,degree = degree, intercept = TRUE)

        A_vv <- A_vv*inside
        A_ww <- A_ww*inside
        A_vw <- A_vw*inside

        A <- list(A_vv = A_vv,
                  A_ww = A_ww,
                  A_vw = A_vw)

        #A <- A_vv+A_ww + 2*A_vw

        A_sqrt <- t(A_vv)%*%A_vv + t(A_ww)%*%A_ww + 2*(t(A_vw)%*%A_vw)

    }
   # print(paste("init",initBeta))
    rawSpline <- initSpline(y=y,knots = innerKnots, degree = degree, intercept = TRUE,dim)
    print("rawSpline")

    if(dim == 1){
      normalize <- res
    } else if(dim ==2){
      normalize <- res^2
    } else {
      message("wrong dimension in constructor of the optimizationObject")
    }

    penalize <- pen(beta, rawSpline = rawSpline, m = m,
                    b = b, lambda = lambda, split = split, A = A_sqrt,
                    prop=prop, dim = dim, res = res,
                    inside = inside, normalize = normalize)


    gradient <- gr(beta,rawSpline = rawSpline, m = m,
                   b = b, lambda = lambda, split = split,A=A,
                   prop=prop, dim = dim, res = res,
                   inside = inside, normalize = normalize)

   # print(paste("pen",penalize))


    return(new_optimizationObject(markRecaptureObject = markRecaptureObject,
                                   initBeta = initBeta,
                                   y=y,
                                   knots = knots,
                                   b = b,
                                   degree = degree,
                                   lambda = lambda,
                                  split = split,
                                  penalize = penalize,
                                  gradient = gradient,
                                  rawSpline = rawSpline,
                                  inside = inside))
}
