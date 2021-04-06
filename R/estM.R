#' estimate migratory connectivity
#'
#' This function estimates the migratory connectivity in space constant over time
#' when survival and raw distribution of dead recoveries for every breeding area is known.
#' @param res_x resolution in space x-axis
#' @param res_y resolution in space y-axis
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @param all boolean: if TRUE only one kernel density estimate will be calculated
#' summarising all breeding areas. Defaults to FALSE.
#' @param dataType character, use "sim" for simulated data, "data" for real world data. Defaults to "sim".
#' @param robust type of estimator used for linear regression to estimate survival. If TRUE
#' a robust regression will be computed (robustbase::lmrob()), if FALSE an ordinary regression
#' (base::lm()). Defaults to TRUE.
#' @param auxiliaryVariable specify, if an auxiliary variable like the at risk-population should be
#' used. Defaults to NULL.#' @return list of vectors with length res_x-1 containing migratory connectivity density of every spot
#' @export
#' @examples estM()
estM <- function(res_x, res_y = res_x, markRecaptureObject,all = FALSE,dataType = "sim",
                 robust = TRUE,auxiliaryVariable = NULL){
  print(auxiliaryVariable)
  if(is.null(auxiliaryVariable)){
    auxiliaryVariable <- matrix(1,ncol=res_y,nrow = res_x)
  }
  print(auxiliaryVariable)
  s_fit <- markRecaptureObject$estimates$s
  breedingAreaNames <- names(markRecaptureObject$breedingAreas)[!grepl("all",names((markRecaptureObject$breedingAreas)))]
  xrange <- markRecaptureObject$winteringArea$window$xrange
  dim <- markRecaptureObject$spatialDim

  if(all){
    lm <- markRecaptureObject$estimates$lm$all
    markRecaptureObject$estimates[["m"]][["all"]] <- exp(lm$intercept-log(1-s_fit))
    if(dim == 1){
      #markRecaptureObject$estimates[["c"]]["all"] <- pracma::romberg(splinefun(markRecaptureObject$estimates[["m"]][["all"]]), a = xrange[1], b = xrange[2])$value#*sum(abs(xrange))
      markRecaptureObject$estimates[["c"]]["all"] <- sum(markRecaptureObject$estimates$m$all)/res_x*sum(abs(xrange))
      markRecaptureObject$estimates[["m"]][["all"]] <- markRecaptureObject$estimates[["m"]][["all"]]/markRecaptureObject$estimates[["c"]]["all"]
    }else if(dim == 2){
        yrange <- markRecaptureObject$winteringArea$window$yrange
      markRecaptureObject$estimates[["c"]]["all"] <- sum(markRecaptureObject$estimates[["m"]][["all"]], na.rm = TRUE)/(res_x*res_y)*sum(abs(xrange))*sum(abs(yrange))
      markRecaptureObject$estimates[["m"]][["all"]] <- markRecaptureObject$estimates[["m"]][["all"]]/markRecaptureObject$estimates[["c"]]["all"]
    }
  } else {

    for(b in breedingAreaNames){
      markRecaptureObject <- estLM(res_x,markRecaptureObject,res_y = res_y,b=b,
                                   dataType = dataType,robust = robust,
                                   auxiliaryVariable = auxiliaryVariable)
      lm <- markRecaptureObject$estimates$lm[[b]]
      markRecaptureObject$estimates[["m"]][[b]] <- exp(lm$intercept-log(1-s_fit))
      if(dim == 1){
        #markRecaptureObject$estimates[["c"]][b] <- pracma::romberg(splinefun(markRecaptureObject$estimates[["m"]][[b]]), a = 0, b=res_x)$value/res_x*sum(abs(xrange))
        markRecaptureObject$estimates[["c"]][b] <- sum(markRecaptureObject$estimates[["m"]][[b]])/res_x*sum(abs(xrange))
        markRecaptureObject$estimates[["m"]][[b]] <- markRecaptureObject$estimates[["m"]][[b]]/markRecaptureObject$estimates[["c"]][b]

      }else if(dim == 2){
        yrange <- markRecaptureObject$winteringArea$window$yrange

        markRecaptureObject$estimates[["c"]][b] <- sum(markRecaptureObject$estimates[["m"]][[b]], na.rm = TRUE)/(res_x*res_y)*sum(abs(xrange))*sum(abs(yrange))
        markRecaptureObject$estimates[["m"]][[b]] <- markRecaptureObject$estimates[["m"]][[b]]/markRecaptureObject$estimates[["c"]][b]

      }
    }
  }

  return(markRecaptureObject)
}
