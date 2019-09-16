#' linear model of kernel density estimate
#'
#' This function estimates the parameters of a linear model for a certain point
#' of the kernel density estimate dependent on time. Linear models are obtained
#' according to the resolution.
#' @param res_x resolution in space
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @return vector of length res_x with survival probabilities dependent on space
#' @export
#' @examples estS()

estLM <- function(res_x,markRecaptureObject,res_y = res_x,b,dataType = "sim",robust = T,
                  auxiliaryVariable = NULL){
  dim <- markRecaptureObject$spatialDim
  kde_all <- markRecaptureObject$kde[[dataType]][[b]]$z
  if(!is.null(auxiliaryVariable)){kde_all <- lapply(kde_all,function(x){x$v/auxiliaryVariable})}
  if(dim == 1){
    kde_all <- sapply(kde_all,function(x) colMeans(x$v))
    res_y <- 1
  }else if(dim == 2){
    kde_all <- sapply(kde_all,with,v)
  }

  lm_fit <- apply(kde_all,1,function(x){
    if(sum(x,na.rm=TRUE)!=0){
      if(robust){
        fit <- robustbase::lmrob(log(x)~as.numeric((as.numeric(colnames(kde_all))-1)))
      }else{fit <- lm(log(x)~as.numeric((as.numeric(colnames(kde_all))-1)))}
      c(coefficients(fit),summary(fit)$r.squared)
    }else{c(NA,NA,NA)}
  })

  markRecaptureObject$estimates[["lm"]][[b]][["intercept"]] <- matrix(lm_fit[1,],ncol = res_x,nrow = res_y,byrow=TRUE)
  markRecaptureObject$estimates[["lm"]][[b]][["slope"]] <- matrix(lm_fit[2,],ncol = res_x,nrow = res_y,byrow=TRUE)
  markRecaptureObject$estimates[["lm"]][[b]][["gof"]] <- matrix(lm_fit[3,],ncol = res_x,nrow = res_y,byrow=TRUE)

  return(markRecaptureObject)
}
