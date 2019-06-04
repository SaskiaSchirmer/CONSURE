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

estS <- function(res_x,markRecaptureObject){

  kde_all <- markRecaptureObject$kde[["all"]]
  T <- markRecaptureObject$observationTime

  s_fit <- numeric()
  for(i in 1:res_x){
    val <- sapply(kde_all$z, function(l) mean(l$v[,i]))
    fit <- lm(log(val)  ~ c(0:(T-1)))
    s_fit[i] <- exp(fit$coefficients[2])
  }
  return(s_fit)
}
