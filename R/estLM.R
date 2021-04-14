#' linear model of kernel density estimate
#'
#' This function estimates the parameters of a linear model for a certain point
#' of the kernel density estimate dependent on time. Linear models are obtained
#' according to the resolution.
#' @inheritParams estS
#' @param fixedSlope numeric. Value for the fixed slope, e.g., to estimate
#' a linear model for the breeding areas separately.
#' @return vector of length res with survival probabilities dependent on space
#' @export
#' @examples estLM()

estLM <- function(markRecaptureObject,b,
                  fixedSlope = NULL,
                  auxiliaryVariable = NULL){
  res <- markRecaptureObject$spatialResolution
  robust <- markRecaptureObject$robust
  dim <- markRecaptureObject$spatialDim
  kde_all <- markRecaptureObject$kde[[b]]$z
  T <- markRecaptureObject$observationTime
  if(b == "all"){
    B <- markRecaptureObject$numberOfBreedingAreas
  }else{ B <- 1}

  if(dim == 1 & !is.null(auxiliaryVariable) & length(auxiliaryVariable) > 1){
    #auxiliaryVariable <- matrix(rep(auxiliaryVariable, res), ncol = res, byrow = TRUE)
  } else if(is.null(auxiliaryVariable)){
    auxiliaryVariable <- matrix(1,ncol=res,nrow = res)#*T*B
  }
  kde_all <- lapply(kde_all,function(x){unname(x$v/as.matrix(auxiliaryVariable))})
  #kde_all <- lapply(kde_all,function(x){unname(x$v/auxiliaryVariable)})

  if(dim == 1){
    kde_all <- sapply(kde_all,function(x) colMeans(x))
    res_y <- 1
  }else if(dim == 2){
    kde_all <- sapply(kde_all,function(x) x)
    res_y <- res
  }

  if(sum(is.infinite(kde_all)) > 0){warning("Infinite values in kernel density estimate.")}
  kde_all[is.infinite(kde_all)] <- NA

  lm_fit <- apply(cbind(kde_all, slope = c(fixedSlope)),1,function(x){
    if(sum(x,na.rm=TRUE)!=0){
      age <- as.numeric(colnames(kde_all))-1

      if(robust){
        if(is.null(fixedSlope)){
          kdeValues <- log(x+10^-200)
          fit <- robustbase::lmrob(kdeValues ~ age, setting="KS2014")
        } else{
          kdeValues <- log(x[-length(x)]+10^-200)
          fixedSlope <- log(x["slope"])
          fit <- robustbase::lmrob(kdeValues ~ 1 + offset(fixedSlope * age), setting="KS2014")
        }
      }else{
        if(is.null(fixedSlope)){
          kdeValues <- log(x+10^-200)
          fit <- lm(kdeValues ~ age)
        } else{
          kdeValues <- log(x[-length(x)]+10^-200)
          fixedSlope <- log(x["slope"])
          fit <- lm(kdeValues ~ 1+ offset(fixedSlope * age))
        }
      }
      if(is.null(fixedSlope)){
        c(coefficients(fit),summary(fit)$r.squared)
      } else{
        c(coefficients(fit)[1],NA,NA)
      }
    }else{c(NA,NA,NA)}
  })

  markRecaptureObject$estimates[["lm"]][[b]][["intercept"]] <- matrix(lm_fit[1,],ncol = res,nrow = res_y)#,byrow=TRUE)
  if(is.null(fixedSlope)){
    markRecaptureObject$estimates[["lm"]][[b]][["slope"]] <- matrix(lm_fit[2,],ncol = res,nrow = res_y)#,byrow=TRUE)
    markRecaptureObject$estimates[["lm"]][[b]][["gof"]] <- matrix(lm_fit[3,],ncol = res,nrow = res_y)#,byrow=TRUE)

  } else{
    markRecaptureObject$estimates[["lm"]][[b]][["slope"]] <- matrix(fixedSlope,ncol = res,nrow = res_y)#,byrow=TRUE)
    markRecaptureObject$estimates[["lm"]][[b]][["gof"]] <- matrix(NA,ncol = res,nrow = res_y)#,byrow=TRUE)
  }

  return(markRecaptureObject)
}
