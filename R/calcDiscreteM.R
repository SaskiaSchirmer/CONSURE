#' True discrete migratory connectivity from a
#' continuous migratory connectivity function.
#'
#' This function integrates the continuous migratory connectivity function
#' for disjoint intervals given as a knot sequence and adds the discrete
#' migratory connectivity values to the
#' markRecaptureObject$breedingAreas[[breedingAreaName]]$mDiscrete.
#' This function is for simulated data only.
#'
#' @param markRecaptureObject an object of the class 'markRecaptureObject'
#' @param knots_prop knot sequence, list with vectors for latitude and 
#' longitude, each vector contains W+1 entries which specify the boundaries 
#' between the discrete non-breeding areas
#'
#' @return markRecaptureObject
#'
#' @export
#' @examples{
#'     kp <-list(latitude = c(0,0.25, 0.5, 0.75,1),
#'         longitude = NULL)
#'     mro <- calcDiscreteM(markRecaptureObject = mro1D, knots_prop = kp)
#' }


calcDiscreteM <- function(markRecaptureObject, knots_prop) {
  dim <- markRecaptureObject$spatialDim
  res <- markRecaptureObject$spatialResolution
  B <- markRecaptureObject$numberOfBreedingAreas

  if (is.null(markRecaptureObject$numberOfDiscreteWinteringAreas)) {
    W <- B
  } else {
    W <- markRecaptureObject$numberOfDiscreteWinteringAreas
  }
  knots <- knots_prop
  breedingAreaNames <- names(markRecaptureObject$breedingAreas)

  if (dim == 1) {
    if (sum(sapply(knots, is.null)) == 1) {
      knots <- unname(unlist(knots))
    } else {
      message("not sure how to use 'knots'")
    }

    bounds <- data.frame(
      lowerBound = knots[1:(length(knots) - 1)],
      upperBound = knots[2:length(knots)]
    )
    nbw <- matrix(NA, ncol = B, nrow = B)

    i <- 1
    for (b in breedingAreaNames) {
      markRecaptureObject$breedingAreas[[b]]$mDiscrete <- 
          apply(bounds, 1, function(x){ 
              stats::integrate(
                  markRecaptureObject$breedingArea[[b]]$migratoryConnectivity, 
                  x[1], x[2])$value
              }
           ) * res
      i <- i + 1
    }

  } else if (dim == 2) {
    boundsLongitude <- data.frame(
      lowerBoundLongitude = knots$longitude[1:(length(knots$longitude) - 1)],
      upperBoundLongitude = knots$longitude[2:length(knots$longitude)]
    )

    boundsLongitude <- boundsLongitude[rep(seq_len(nrow(boundsLongitude)), 
                                           each = length(knots$latitude) - 1), ]

    boundsLatitude <- data.frame(
      lowerBoundLatitude = knots$latitude[1:(length(knots$latitude) - 1)],
      upperBoundLatitude = knots$latitude[2:length(knots$latitude)]
    )

    boundsLatitude <- boundsLatitude[rep(seq_len(nrow(boundsLatitude)), 
                                         length(knots$longitude) - 1), ]

    bounds <- data.frame(cbind(boundsLongitude, boundsLatitude))
    nbw <- matrix(NA, ncol = W, nrow = B)

    i <- 1

    for (b in breedingAreaNames) {
      markRecaptureObject$breedingAreas[[b]]$mDiscrete <- apply(
          bounds, 1, function(x){
              cubature::adaptIntegrate(
                  markRecaptureObject$breedingAreas[[b]]$migratoryConnectivity, 
                  c(x[1], x[3]), c(x[2], x[4])
              )$integral
          } 
      ) * res^2
      i <- i + 1
    }
  } else {
    message("not known how to calculate discrete migratory connectivity for this number of dimension.")
  }

  return(markRecaptureObject)
}
