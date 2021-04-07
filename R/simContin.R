#' simulation function for mark-recovery setting
#'
#' This function allows you to simulate data of dead recoveries with specified
#' survival, migratory connectivity and recovery probability in 1D- or 2D-space and time
#' using rejection sampling.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @return returns object of class markRecaptureObject with added simulated recoveryData:
#' list of vector k: number of recovered dividuals and list eta with B entries, every entry containing
#' a 2xk - data.frame of space and time of every recovery
#' @export
#' @examples simContin()

simContin <- function(markRecaptureObject){
  eta <- list()
  k <- numeric()
  B <- markRecaptureObject$numberOfBreedingAreas
  T <- markRecaptureObject$observationTime
  breedingAreaNames <- names(markRecaptureObject$breedingAreas)[names(markRecaptureObject$breedingAreas) != "all"]

  for(b in breedingAreaNames){
    # calculate probability to be not seen independent of space and time for every breeding area
    p <- 1-p_nf(b,markRecaptureObject)
    # 1st step: simulate count of found individuals
    k[b] <- rbinom(1,markRecaptureObject$breedingAreas[[b]]$markedInds,p)

    # 2nd step: sample data from subdensity
    # using rejection sampling

    #dg <- function(x) prod(c(dunif(x[1], min = 0, max = 1),dtrunc(x[2],"geom",0,10, prob = 0.2)))
    #rg <- function(n) c(runif(n, min = 0, max = 1),rtrunc(n,"geom",0,10, prob = 0.2))
    if(markRecaptureObject$spatialDim != 1){
      f_f2 <- function(x){
          f_f(w = c(x[1],x[2]), t= x[3], b=b,markRecaptureObject,p)
      }

      dg <- function(x) prod(c(dbeta(x[1],shape1 =  1, shape2 = 2),
                               dbeta(x[2],shape1 =  1, shape2 = 2),
                               truncdist::dtrunc(x[3],"geom",0,T, prob = 0.2)))+0.0000000001
      rg <- function(n) c(rbeta(n, shape1 =  1, shape2 = 2),
                          rbeta(n, shape1 =  1, shape2 = 2),
                          truncdist::rtrunc(n,"geom",0,T, prob = 0.2))
      cnames <- c("longitude","latitude","time")
    }else{
      f_f2 <- function(x){
          f_f(w = x[1], t= x[2], b=b, markRecaptureObject,p)
      }

      dg <- function(x) prod(c(dbeta(x[1],shape1 =  1, shape2 = 2),
                               truncdist::dtrunc(x[2],"geom",0,T, prob = 0.2)))+0.0000000001
      rg <- function(n) c(rbeta(n, shape1 =  1, shape2 = 2),
                          truncdist::rtrunc(n,"geom",0,T, prob = 0.2))

      cnames <- c("longitude","time")
    }

    # dg <- function(x) prod(c(dunif(x[1], min = 0, max = 1),dunif(x[2],min=1,max=10)))
    #rg <- function(n) c(runif(n, min = 0, max = 1),sample(1:10,n, replace = TRUE))

    eta[[b]] <- SimDesign::rejectionSampling(k[b]+1, df = f_f2, dg = dg, rg = rg, M=10)
    eta[[b]] <- eta[[b]][-nrow(eta[[b]]),]
    colnames(eta[[b]]) <- cnames
  }
  markRecaptureObject$winteringArea$recoveryData <- eta

  for(b in breedingAreaNames){
    markRecaptureObject$breedingAreas[[b]]$recoveryData <- eta[[b]]
    markRecaptureObject$breedingAreas[[b]]$numberOfRecoveries <- k[b]
  }
  markRecaptureObject$breedingAreas[["all"]]$recoveryData <- list(do.call("rbind",eta))
  markRecaptureObject$breedingAreas[["all"]]$numberOfRecoveries <- sum(k)

  return(markRecaptureObject)
}

# todo: add possibility to add different proxy-functions
