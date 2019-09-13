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
  dim <- markRecaptureObject$spatialDim
  kde_all <- markRecaptureObject$kde[[dataType]][["all"]]$z
  if(dim == 1){
    kde_all <- sapply(kde_all,function(x) colMeans(x$v))
    res_y <- 1
  }else if(dim == 2){
    kde_all <- sapply(kde_all,with,v)
  }
  #kde_all$row <- rep(1:res_x,res_y)
  #kde_all$col <- rep(1:res_y,each = res_x)

  #T <- markRecaptureObject$observationTime
  #if(identical(markRecaptureObject$winteringArea$window$yrange,c(0,0))) res_y <- 1

  s_fit <- apply(kde_all,1,function(x){
    if(sum(x,na.rm=TRUE)!=0){
      fit <- lm(log(x)~as.numeric(colnames(kde_all)))
      c(exp(coefficients(fit)[2]),summary(fit)$r.squared)
    }else{c(NA,NA)}
  })

  markRecaptureObject$estimates[["s"]] <- matrix(s_fit[1,],ncol = res_x,nrow = res_y,byrow=TRUE)
  markRecaptureObject$estimates[["rSquaredOfS"]] <- matrix(s_fit[2,],ncol = res_x,nrow = res_y,byrow = TRUE)

  #s_fit <- matrix(NA,res_x,res_y)
  #for(i in 1:res_x){
   # for(j in 1:res_y){
    #  if(sum(is.na(sapply(kde_all$z, function(x) x[j,i]))) == 0){
     #   if(res_y == 1){val <- sapply(kde_all$z, function(l) mean(l[,i]))}else{
      #    val <- sapply(kde_all$z, function(l) l[j,i])
       # }
        #fit <- lm(log(val)  ~ c(0:(T-1)))
        #s_fit[i,j] <- exp(fit$coefficients[2])
        #}
      #}
  #}
  #markRecaptureObject$estimates[["s"]] <- s_fit
  return(markRecaptureObject)
}
