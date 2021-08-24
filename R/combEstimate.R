#' Function to get combined migratory connectivity estimate.
#'
#' This function uses an optimization procedure to correct the continuous migratory
#' connectivity estimator by the discrete migratory connectivity estimator. Throws an error
#' if convergence is not achieved.
#' @param optimizationObject an object of the class 'optimizationObject' (s. constructors)
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
#' @examples{
#'     oO <- optimizationObject(markRecaptureObject = mro1DIncreasing$mro,
#'         b = "all",
#'         split = mro1DIncreasing$split,
#'         lambda  = c(.05,300))
#'
#'     mro <- combEstimate(optimizationObject = oO)
#' }

combEstimate <- function(optimizationObject,
                         startTimes = 1,
                         maxit = 100000,
                         reltol = 1e-8,
                         changeR = FALSE){

    markRecaptureObject <- optimizationObject$markRecaptureObject
    dim <- markRecaptureObject$spatialDim

    if(dim == 1){
      inside <- colSums(optimizationObject$inside) > 0
    } else{
      inside <- optimizationObject$inside
    }

    normalize <- sum(inside, na.rm = TRUE)

    fVonX <- matrix(NA, ncol = startTimes, nrow = dim(optimizationObject$rawSpline)[1])
    val <- numeric(startTimes)
    convergence <- numeric(startTimes)

    for(i in 1:startTimes){

        print(paste("startCombEst",i))
        optBeta <- stats::optim(par = optimizationObject$initBeta(),
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
            hVonX[!inside] <- NA
            fVonX[,i] <- exp(hVonX)/sum(exp(hVonX), na.rm = TRUE)*normalize

            val[i] <- NA
         }else{

             hVonX <- (optimizationObject$rawSpline %*%optBeta$par)
             hVonX[!inside] <- NA
             fVonX[,i] <- exp(hVonX)/sum(exp(hVonX), na.rm = TRUE)*normalize
             val[i] <- optBeta$value
             convergence[i] <- (optBeta$convergence == 0)
         }
    }

    if(sum(convergence) != 0){
      f <-  markRecaptureObject$estimates$mCombined[[optimizationObject$b]] <- matrix(fVonX[,which.min(val)],
                                                                                      ncol = dim(markRecaptureObject$estimates$s)[2])

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
          markRecaptureObject$estimates$mCorrected[[b]] <- exp(
            markRecaptureObject$estimates$lm[[b]]$intercept)/
            markRecaptureObject$estimates$rCombined/
            (1-markRecaptureObject$estimates$s)*
            markRecaptureObject$breedingAreas[[b]]$numberOfRecoveries/
            markRecaptureObject$breedingAreas[[b]]$markedInds

          markRecaptureObject$estimates$mCorrected[[b]] <- markRecaptureObject$estimates$mCorrected[[b]]/sum(markRecaptureObject$estimates$mCorrected[[b]], na.rm = TRUE)*normalize
        }
      }

      return(list(fVonX = fVonX, val = val,
                  markRecaptureObject = markRecaptureObject,
                  convergence = convergence))
    } else {
        message("convergence never achieved")
    }
}
