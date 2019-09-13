#' estimate migratory connectivity
#'
#' This function estimates the migratory connectivity in space constant over time
#' when survival and raw distribution of dead recoveries for every breeding area is known.
#' @param B integer, number of breeding areas
#' @param res_x resolution in space x-axis
#' @param res_y resolution in space y-axis
#' @param kde list of B values created by sparr::spattemp.density
#' (see ?sparr::spattemp.density for details): raw distribution of dead recoveries
#' @param all boolean: if TRUE only one kernel density estimate will be calculated
#' summarising all breeding areas. Defaults to FALSE.
#' @return list of vectors with length res_x-1 containing migratory connectivity density of every spot
#' @export
#' @examples estM()
estM <- function(res_x, res_y = 0, markRecaptureObject,all = FALSE,dataType = "sim",
                 auxiliaryVariable = NULL){
  if(is.null(auxiliaryVariable)){auxiliaryVariable <- matrix(1,ncol=res_y,nrow = res_x)}
  kde <- markRecaptureObject$kde[[dataType]]
  B <- markRecaptureObject$numberOfBreedingAreas
  T <- markRecaptureObject$observationTime
  s_fit <- markRecaptureObject$estimates$s
  breedingAreaNames <- names(markRecaptureObject$breedingAreas)[!grepl("all",names((markRecaptureObject$breedingAreas)))]

  m_fit <- list()
  if(all){
    if(markRecaptureObject$spatialDim == 1){
      for(i in 1:res_x){
        val <- sapply(kde[["all"]]$z, function(l) mean(l[,i]))
        fit <- lm(log(val)  ~ c(0:(T-1)))
        markRecaptureObject$estimates[["m"]][["all"]][i] <- exp(fit$coefficients[1]-log(1-s_fit[i]+0.000000001))
      }
      markRecaptureObject$estimates[["c"]]["all"] <- pracma::romberg(splinefun(markRecaptureObject$estimates[["m"]][["all"]]), a = 0, b=res_x)$value/res_x
      markRecaptureObject$estimates[["m"]][["all"]] <- markRecaptureObject$estimates[["m"]][["all"]]/markRecaptureObject$estimates[["c"]]["all"]
    }else if(markRecaptureObject$spatialDim == 2){
        markRecaptureObject$estimates[["m"]][["all"]] <- matrix(NA, ncol = res_y, nrow = res_x)
      for(i in 1:res_x){
        for(j in 1:res_y){
          if(sum(is.na(sapply(kde[["all"]]$z, function(x) x[j,i]))) == 0){
            if(!is.na(auxiliaryVariable[j,i]) & auxiliaryVariable[j,i] != 0){
              val <- sapply(kde[["all"]]$z, function(l) l[j,i]/auxiliaryVariable[j,i])
              fit <- lm(log(val)  ~ c(0:(T-1)))
              markRecaptureObject$estimates[["m"]][["all"]][i,j] <- exp(fit$coefficients[1]-log(1-s_fit[i,j]+0.000000001))
            }
          }
        }
      }
      markRecaptureObject$estimates[["c"]]["all"] <- sum(markRecaptureObject$estimates[["m"]][["all"]], na.rm = TRUE)/(res_x*res_y)
      markRecaptureObject$estimates[["m"]][["all"]] <- markRecaptureObject$estimates[["m"]][["all"]]/markRecaptureObject$estimates[["c"]]["all"]
    }
  } else{
    if(markRecaptureObject$spatialDim == 1){
      for(b in breedingAreaNames){
        markRecaptureObject$estimates[["m"]][[b]] <- numeric()

        for(i in 1:res_x){
          val <- sapply(kde[[b]]$z, function(l) mean(l[,i]))
          fit <- lm(log(val)  ~ c(0:(T-1)))
          markRecaptureObject$estimates[["m"]][[b]][i] <- exp(fit$coefficients[1]-log(1-s_fit[i]+0.000000001))
        }
        markRecaptureObject$estimates[["c"]][b] <- pracma::romberg(splinefun(markRecaptureObject$estimates[["m"]][[b]]), a = 0, b=res_x)$value/res_x
        markRecaptureObject$estimates[["m"]][[b]] <- markRecaptureObject$estimates[["m"]][[b]]/markRecaptureObject$estimates[["c"]][b]
      }
    } else{
      for(b in breedingAreaNames){
        markRecaptureObject$estimates[["m"]][[b]] <- matrix(NA, ncol = res_y, nrow = res_x)

        for(i in 1:res_x){
          for(j in 1:res_y){
            if(sum(is.na(sapply(kde[[b]]$z, function(x) x[j,i]))) == 0){
              if(!is.na(auxiliaryVariable[j,i]) & auxiliaryVariable[j,i] != 0){
                val <- sapply(kde[[b]]$z, function(l) l[j,i]/auxiliaryVariable[j,i])
                fit <- lm(log(val)  ~ c(0:(T-1)))
                markRecaptureObject$estimates[["m"]][[b]][i,j] <- exp(fit$coefficients[1]-log(1-s_fit[i,j]+0.000000001))
              }
            }
          }
        }
        markRecaptureObject$estimates[["c"]][b] <- sum(markRecaptureObject$estimates[["m"]][[b]], na.rm = TRUE)/(res_x*res_y)
        markRecaptureObject$estimates[["m"]][[b]] <- markRecaptureObject$estimates[["m"]][[b]]/markRecaptureObject$estimates[["c"]][b]
      }
    }
  }

  return(markRecaptureObject)
}
