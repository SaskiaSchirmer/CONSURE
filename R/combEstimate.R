#' Function to get combined migratory connectivity estimate.
#'
#' This function uses an optimization procedure to correct the continuous migratory
#' connectivity estimator by the discrete migratory connectivity estimator. Throws an error
#' if convergence is not achieved.
#' @param optimizationObject an object of the class 'optimizationObject' (s. constructors)
#' @param markRecaptureObject an object of the class 'markRecaptureObject'
#' @param startTimes number of repetitions of the optimization procedure. Defaults to 1.
#' @param maxit numbers of iterations used by the control argument of optim,
#' defaults to 10000, see ?optim for details.
#' @param reltol Relative convergence tolerance to stop optimization algorithm.
#' Used by the control argument of optim,
#' defaults to 1e-8, see ?optim for details.
#' @param changeR logical. If TRUE, it estimates the combined version of the recovery
#' probability and adds it to markRecaptureObject$estimates$rCombined. Should be commonly
#' set to TRUE if the breeding area argument in the optimization Object is 'all' and FALSE
#' otherwise. Defaults to FALSE.
#'
#' @return fVonX data.frame with estimates of every optimization
#' @return val numeric vector of all optimizations
#' @return markRecaptureObject
#'
#' @export
#' @examples combEstimate()

combEstimate <- function(optimizationObject,
                         markRecaptureObject,
                         startTimes = 1,
                         maxit = 100000,
                         reltol = 1e-8,
                         changeR = FALSE){

    dim <- markRecaptureObject$spatialDim

    if(dim == 1){
        res <- markRecaptureObject$spatialResolution
    } else if(dim == 2) {
        res <- markRecaptureObject$spatialResolution^2
    } else {message("Dimension not available.")}

    fVonX <- matrix(NA, ncol = startTimes, nrow = dim(optimizationObject$rawSpline)[1])
    val <- numeric(startTimes)
    convergence <- numeric(startTimes)

    for(i in 1:startTimes){

        print(paste("startCombEst",i))
        optBeta <- optim(par = optimizationObject$initBeta(),
                         method = "BFGS",
                     fn= optimizationObject$penalize,
                     gr = optimizationObject$gradient,
                     control = list(maxit = maxit,
                                    reltol = reltol))

        print(optBeta$value)
        print(paste("optimization done",i))

        if(optBeta$convergence != 0){
            message("convergence not achieved")

            hVonX <- (optimizationObject$rawSpline %*%optBeta$par)

            fVonX[,i] <- exp(hVonX)/sum(exp(hVonX))*res*optimizationObject$inside

            val[i] <- NA
         }else{

             hVonX <- (optimizationObject$rawSpline %*%optBeta$par)
             fVonX[,i] <- exp(hVonX)/sum(exp(hVonX))*res*optimizationObject$inside
             val[i] <- optBeta$value
             convergence[i] <- (optBeta$convergence == 0)
         }
    }

    if(sum(convergence) != 0){
      f <-  markRecaptureObject$estimates$mCombined[[optimizationObject$b]] <- matrix(fVonX[,which.min(val)], ncol = dim(markRecaptureObject$estimates$s)[2])

      if(changeR){

        markRecaptureObject$estimates$rCombined <- matrix(
          exp(
              markRecaptureObject$estimates$lm[[optimizationObject$b]]$intercept
          )/
          f/
          (1-markRecaptureObject$estimates$s)*
          markRecaptureObject$breedingAreas[[optimizationObject$b]]$numberOfRecoveries/
          markRecaptureObject$breedingAreas[[optimizationObject$b]]$markedInds,
          ncol = dim(markRecaptureObject$estimates$s)[2]
        )

        breedingAreaNames <- names(markRecaptureObject$breedingAreas)

        for(b in breedingAreaNames){
          markRecaptureObject$estimates$mCorrected[[b]] <- exp(markRecaptureObject$estimates$lm[[b]]$intercept)/markRecaptureObject$estimates$rCombined/(1-markRecaptureObject$estimates$s)*markRecaptureObject$breedingAreas[[b]]$numberOfRecoveries/markRecaptureObject$breedingAreas[[b]]$markedInds
        }
      }

      return(list(fVonX = fVonX, val = val, markRecaptureObject = markRecaptureObject, convergence = convergence))
    } else {
        message("convergence never achieved")
    }
}
