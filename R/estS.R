#' survival estimator
#'
#' This function estimates the survival from a kernel density estimate of the data
#' of recovered individuals. It uses the data of all breeding areas at once.
#' @param res_x resolution in space
#' @param kde_all list of one value created by sparr::spattemp.density
#' (see ?sparr::spattemp.density for details): raw distribution of dead recoveries
#' @param T integer: length of observation period
#' @return vector of length res_x with survival probabilities dependent on space
#' @export
#' @examples estS()

estS <- function(res_x,kde_all,T){
  s_fit <- numeric()
  for(i in 1:res_x){
    val <- sapply(kde_all[[1]]$z, function(l) mean(l[,i]))
    fit <- lm(log(val)  ~ c(0:(T-1)))
    s_fit[i] <- exp(fit$coefficients[2])
  }
  return(s_fit)
}
