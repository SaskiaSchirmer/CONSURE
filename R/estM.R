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
estM <- function(res_x, res_y = 0, markRecaptureObject,all = FALSE,dataType = "sim"){
  kde <- markRecaptureObject$kde[[dataType]]
  B <- markRecaptureObject$numberOfBreedingAreas
  T <- markRecaptureObject$observationTime
  s_fit <- markRecaptureObject$estimates$s

  m_fit <- list()
  if(all){
    if(markRecaptureObject$spatialDim == 1){
      for(i in 1:res_x){
        val <- sapply(kde[["all"]]$z, function(l) mean(l[,i]))
        fit <- lm(log(val)  ~ c(0:(T-1)))
        markRecaptureObject$estimates[["m"]][["all"]][i] <- exp(fit$coefficients[1]-log(1-s_fit[i]+0.000000001))
      }
    } else{
        markRecaptureObject$estimates[["m"]][["all"]] <- matrix(NA, ncol = res_y, nrow = res_x)
      for(i in 1:res_x){
        for(j in 1:res_y){
          val <- sapply(kde[["all"]]$z, function(l) l[i,j])
          fit <- lm(log(val)  ~ c(0:(T-1)))
          markRecaptureObject$estimates[["m"]][["all"]][i,j] <- exp(fit$coefficients[1]-log(1-s_fit[i,j]+0.000000001))
        }
      }
    }
  } else{
    if(markRecaptureObject$spatialDim == 1){
      for(b in 1:B){
        markRecaptureObject$estimates[["m"]][[paste("b",b,sep = "")]] <- numeric()

        for(i in 1:res_x){
          val <- sapply(kde[[paste("b",b,sep = "")]]$z, function(l) mean(l[,i]))
          fit <- lm(log(val)  ~ c(0:(T-1)))
          markRecaptureObject$estimates[["m"]][[paste("b",b,sep = "")]][i] <- exp(fit$coefficients[1]-log(1-s_fit[i]+0.000000001))
        }
      }
    } else{
      for(b in 1:B){
        markRecaptureObject$estimates[["m"]][[paste("b",b,sep = "")]] <- matrix(NA, ncol = res_y, nrow = res_x)

        for(i in 1:res_x){
          for(j in 1:res_y){
            val <- sapply(kde[[paste("b",b,sep = "")]]$z, function(l) l[i,j])
            fit <- lm(log(val)  ~ c(0:(T-1)))
            markRecaptureObject$estimates[["m"]][[paste("b",b,sep = "")]][i,j] <- exp(fit$coefficients[1]-log(1-s_fit[i,j]+0.000000001))
          }
        }
      }
    }
  }
  return(markRecaptureObject)
}
