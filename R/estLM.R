#' linear model of kernel density estimate
#'
#' This function estimates the parameters of a linear model for a certain point
#' of the kernel density estimate dependent on time. Linear models are obtained
#' according to the resolution.
#' @inheritParams estS
#' @return vector of length res with survival probabilities dependent on space
#' @export
#' @examples estLM()

estLM <- function(markRecaptureObject,b,
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

  lm_fit <- apply(kde_all,1,function(x){
    if(sum(x,na.rm=TRUE)!=0){
      if(robust){
        fit <- robustbase::lmrob(log(x+10^-200)~as.numeric((as.numeric(colnames(kde_all))-1)), method="MM", init=robustbase::lmrob.lar, psi="lqq")
      }else{fit <- lm(log(x+10^-200)~as.numeric((as.numeric(colnames(kde_all))-1)))}
      c(coefficients(fit),summary(fit)$r.squared)
    }else{c(NA,NA,NA)}
  })

  markRecaptureObject$estimates[["lm"]][[b]][["intercept"]] <- matrix(lm_fit[1,],ncol = res,nrow = res_y)#,byrow=TRUE)
  markRecaptureObject$estimates[["lm"]][[b]][["slope"]] <- matrix(lm_fit[2,],ncol = res,nrow = res_y)#,byrow=TRUE)
  markRecaptureObject$estimates[["lm"]][[b]][["gof"]] <- matrix(lm_fit[3,],ncol = res,nrow = res_y)#,byrow=TRUE)

  return(markRecaptureObject)
}
