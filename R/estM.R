#' estimate migratory connectivity
#'
#' This function estimates the migratory connectivity in space constant over time
#' when survival and raw distribution of dead recoveries for every breeding area is known.
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param all boolean: if TRUE only one kernel density estimate will be calculated
#' summarising all breeding areas. Defaults to FALSE.
#' @param auxiliaryVariable specify, if an auxiliary variable like the at risk-population should be
#' used. Defaults to NULL.#' @return list of vectors with length res-1 containing migratory connectivity density of every spot
#' @export
#' @examples mro <- estM(mro1D, all = TRUE)
estM <- function(markRecaptureObject,all = FALSE,
                 auxiliaryVariable = NULL){

  robust <- markRecaptureObject$robust

  res <- markRecaptureObject$spatialResolution

  if(is.null(auxiliaryVariable)){
    auxiliaryVariable <- matrix(1,ncol=res,nrow = res)
  }

  s_fit <- markRecaptureObject$estimates$s
  breedingAreaNames <- names(markRecaptureObject$breedingAreas)[!grepl("all",names((markRecaptureObject$breedingAreas)))]
  xrange <- markRecaptureObject$winteringArea$window$xrange
  dim <- markRecaptureObject$spatialDim
  normalize <- sum(markRecaptureObject$inside, na.rm = TRUE)

  if(all){
    lm <- markRecaptureObject$estimates$lm$all
    markRecaptureObject$estimates[["m"]][["all"]] <- exp(lm$intercept-log(1-s_fit))
    if(dim == 1){
      #markRecaptureObject$estimates[["c"]]["all"] <- pracma::romberg(splinefun(markRecaptureObject$estimates[["m"]][["all"]]), a = xrange[1], b = xrange[2])$value#*sum(abs(xrange))
     # markRecaptureObject$estimates[["c"]]["all"] <- sum(markRecaptureObject$estimates$m$all)/res*sum(abs(xrange))
      markRecaptureObject$estimates[["c"]]["all"] <- sum(markRecaptureObject$estimates$m$all)/normalize
      #markRecaptureObject$estimates[["c"]]["all"] <- sum(markRecaptureObject$estimates$m$all)

      markRecaptureObject$estimates[["m"]][["all"]] <- markRecaptureObject$estimates[["m"]][["all"]]/markRecaptureObject$estimates[["c"]]["all"]*markRecaptureObject$inside
    }else if(dim == 2){
        yrange <- markRecaptureObject$winteringArea$window$yrange
      #markRecaptureObject$estimates[["c"]]["all"] <- sum(markRecaptureObject$estimates[["m"]][["all"]], na.rm = TRUE)/(res*res)*sum(abs(xrange))*sum(abs(yrange))
      markRecaptureObject$estimates[["c"]]["all"] <- sum(markRecaptureObject$estimates[["m"]][["all"]], na.rm = TRUE)/normalize
      #markRecaptureObject$estimates[["c"]]["all"] <- sum(markRecaptureObject$estimates[["m"]][["all"]], na.rm = TRUE)
      markRecaptureObject$estimates[["m"]][["all"]] <- markRecaptureObject$estimates[["m"]][["all"]]/markRecaptureObject$estimates[["c"]]["all"]*markRecaptureObject$inside
    }
  } else {

    for(b in breedingAreaNames){
      markRecaptureObject <- estLM(markRecaptureObject,b=b,
                                   fixedSlope = markRecaptureObject$estimates$s,
                                   auxiliaryVariable = auxiliaryVariable)
      lm <- markRecaptureObject$estimates$lm[[b]]
      markRecaptureObject$estimates[["m"]][[b]] <- exp(lm$intercept-log(1-s_fit))
      if(dim == 1){
        #markRecaptureObject$estimates[["c"]][b] <- pracma::romberg(splinefun(markRecaptureObject$estimates[["m"]][[b]]), a = 0, b=res)$value/res*sum(abs(xrange))
       # markRecaptureObject$estimates[["c"]][b] <- sum(markRecaptureObject$estimates[["m"]][[b]])/res*sum(abs(xrange))
        markRecaptureObject$estimates[["c"]][b] <- sum(markRecaptureObject$estimates[["m"]][[b]])/normalize
       # markRecaptureObject$estimates[["c"]][b] <- sum(markRecaptureObject$estimates[["m"]][[b]])
        markRecaptureObject$estimates[["m"]][[b]] <- markRecaptureObject$estimates[["m"]][[b]]/markRecaptureObject$estimates[["c"]][b]*markRecaptureObject$inside

      }else if(dim == 2){
        yrange <- markRecaptureObject$winteringArea$window$yrange
        #markRecaptureObject$estimates[["c"]][b] <- sum(markRecaptureObject$estimates[["m"]][[b]], na.rm = TRUE)/(res*res)*sum(abs(xrange))*sum(abs(yrange))
        markRecaptureObject$estimates[["c"]][b] <- sum(markRecaptureObject$estimates[["m"]][[b]], na.rm = TRUE)/normalize
        #markRecaptureObject$estimates[["c"]][b] <- sum(markRecaptureObject$estimates[["m"]][[b]], na.rm = TRUE)
        markRecaptureObject$estimates[["m"]][[b]] <- markRecaptureObject$estimates[["m"]][[b]]/markRecaptureObject$estimates[["c"]][b]*markRecaptureObject$inside

      }
    }
  }

  return(markRecaptureObject)
}
