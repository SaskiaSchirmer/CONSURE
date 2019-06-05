#' survival estimator
#'
#' This function estimates the survival from a kernel density estimate of the data
#' of recovered individuals. It uses the data of all breeding areas at once.
#' @param res_x resolution in space
#' @param markRecaptureObject object of class markRecaptureObject
#' (see markRecaptureObject())
#' @return vector of length res_x with survival probabilities dependent on space
#' @export
#' @examples estS()

estS <- function(res_x,markRecaptureObject,res_y = res_x,dataType = "sim"){

  kde_all <- markRecaptureObject$kde[[dataType]][["all"]]
  T <- markRecaptureObject$observationTime
  if(identical(markRecaptureObject$winteringArea$window$yrange,c(0,0))) res_y <- 1

  s_fit <- matrix(NA,res_x,res_y)
  for(i in 1:res_x){
    for(j in 1:res_y){
      if(res_y == 1){val <- sapply(kde_all$z, function(l) mean(l$v[,i]))}else{
        val <- sapply(kde_all$z, function(l) l$v[j,i])
      }
      fit <- lm(log(val)  ~ c(0:(T-1)))
      s_fit[i,j] <- exp(fit$coefficients[2])
    }
  }
  return(s_fit)
}
