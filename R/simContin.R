#' 1D simulation function
#'
#' This function allows you to simulate data of dead recoveries with specified
#' survival, migratory connectivity and recovery probability in 1D-space and time
#' using rejection sampling.
#' @param B integer: total number of breeding areas
#' @param p_nf vector with length B: probability of not seen
#' individuals from breeding area b independent of place of recovery (marginalized over whole wintering area)
#' @param n vector of length B: number of marked individuals per breeding area
#' @param lb lower bound of wintering area
#' @param ub upper bound of wintering area
#' @param s function for survival probability
#' @param m function for migratory connectivity, integrates to 1
#' @param r constant recovery probability
#' @param T integer: length of observation period
#' @return list of vector k: number of recovered dividuals and list eta with B entries, every entry containing
#' a 2xk - data.frame of space and time of every recovery
#' @export
#' @examples simContin()

simContin <- function(B, p_nf, n, lb, ub, s, m, r, T){
  eta <- list()
  k <- numeric()

  for(b in 1:B){
    # 1st step: simulate count of found individuals
    k[b] <- rbinom(1,n[b],1-p_nf[b])

    # 2nd step: sample data from subdensity
    # using rejection sampling
    f_f2 <- function(x){
      f_f(w = x[1], t= x[2], b=b, lb = lb, ub = ub, s,m,r,T)
    }

    #dg <- function(x) prod(c(dunif(x[1], min = 0, max = 1),dtrunc(x[2],"geom",0,10, prob = 0.2)))
    #rg <- function(n) c(runif(n, min = 0, max = 1),rtrunc(n,"geom",0,10, prob = 0.2))

    dg <- function(x) prod(c(dbeta(x[1],shape1 =  1, shape2 = 2),truncdist::dtrunc(x[2],"geom",0,10, prob = 0.2)))+0.0000000001
    rg <- function(n) c(rbeta(n, shape1 =  1, shape2 = 2),truncdist::rtrunc(n,"geom",0,10, prob = 0.2))

    # dg <- function(x) prod(c(dunif(x[1], min = 0, max = 1),dunif(x[2],min=1,max=10)))
    #rg <- function(n) c(runif(n, min = 0, max = 1),sample(1:10,n, replace = TRUE))

    eta[[b]] <-SimDesign::rejectionSampling(k[b]+1, df = f_f2, dg = dg, rg = rg, M=10)
    eta[[b]] <- eta[[b]][-nrow(eta[[b]]),]
  }
  return(list(eta=eta,k=k))
}

# todo: add possibility to add different proxy-functions
