#' estimate migratory connectivity
#'
#' This function estimates the migratory connectivity in space constant over time
#' when survival and raw distribution of dead recoveries for every breeding area is known.
#' @param B integer, number of breeding areas
#' @param res_x resolution in space
#' @param kde list of B values created by sparr::spattemp.density
#' (see ?sparr::spattemp.density for details): raw distribution of dead recoveries
#' @param all boolean: if TRUE only one kernel density estimate will be calculated
#' summarising all breeding areas. Defaults to FALSE.
#' @return list of vectors with length res_x-1 containing migratory connectivity density of every spot
#' @export
#' @examples estF()
estM <- function(B, res_x, kde,s_fit, all = FALSE){
  f_fit <- list()
  if(all) B <- 1
  for(b in 1:B){
    f_fit[[b]] <- numeric()

    for(i in 1:(res_x-1)){
      val <- sapply(kde[[b]]$z, function(l) mean(l[,i]))
      fit <- lm(log(val)  ~ c(0:(T-1)))
      f_fit[[b]][i] <- exp(fit$coefficients[1]-log(1-s_fit[i]+0.000000001))
    }
  }
  return(f_fit)
}
