

new_winteringArea1D <- function(boundary = numeric(2L),
                                survival,
                                recovery){
  stopifnot(is.numeric(boundary))
  stopifnot(is.function(survival))
  stopifnot(is.function(recovery))
  structure(boundary, class = "winteringArea1D")
}


w <- new_winteringArea1D(c(0,1), survival = function(w){0.5*w+.4}, r = function(w){0.01})


new_breedingArea <- function(markedInds = integer(),
                             winteringArea,
                             migratoryConnectivity){
  stopifnot(is.integer(markedInds))
  stopifnot(class(winteringArea) == "winteringArea1D")
  stopifnot(is.function(migratoryConnectivity))
  structure(markedInds, class = "breedingArea")
}

new_breedingArea(1000L)


new_observationCircumstances <- function(numberOfBreedingAreas = integer(),
                                     observationTime = integer(),
                                     breedingAreas = list(),
                                     winteringArea = winteringArea1D()){
  stopifnot(is.integer(numberOfBreedingAreas))
  stopifnot(is.integer(observationTime))
  stopifnot(is.list(breedingAreas))
  stopifnot(class(winteringArea) == "winteringArea1D")

  structure(list(B = numberOfBreedingAreas,
                 T = observationTime,
                 breedingAreas = breedingAreas,
                 winteringArea = winteringArea))
}

obs <- new_observationCircumstances(5L,10L,rep(list(new_breedingArea(1000L)),5),new_winteringArea1D(c(0,1)))
