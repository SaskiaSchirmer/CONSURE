wd <- getwd()
setwd("..")
devtools::install("continousSpatioTemporalMarkRecapture/")
library(continousSpatioTemporalMarkRecapture)

B <- 5     # number of breeding areas
#f_mean <- 0.3
f_mean <- seq(0.1,0.9,length.out = B) # preparation to get different distributions per breeding area
f_sd <- 0.3
n <- ceiling(runif(B,100000,1000000))    # number of marked individuals in 1 breeding area
T <- 10         # observation time
res_x <- 100

##############################################
### First scenario: continuous in 1 dimension
##############################################
### assume that wintering area W is the interval [0,1]

### the breeding area specific distribution of individuals
### in the wintering area is a truncated normal as integral over W is 1
f <- function(b,w){
  truncnorm::dtruncnorm(w,0,1, mean = f_mean[b], sd = f_sd)}

### s is a linear function in w
s <- function(w){0.5*w+.4}

### r is a constant (for identifiability reasons)
r <- 0.01

plotFsr(f,s,r,T)

# not seen
p_nf <- numeric()

for(b in 1:B){
  p_nf[b] <- integrate(f = f_nf_sub,lower = 0, upper = 1, b = b, s= s, m=f, r=r, T = T)$value
}

out <- simContin(B,p_nf,n,lb=0, ub=1, s,m=f,r,T)

eta <- out$eta
k <- out$k
res_x <- 100
kde_all <- estKDE(eta, B,T,res_x, all = TRUE)
kde <- estKDE(eta,B,T,res_x)

source("plotKDE.R")
plotKDE(1,kde_all,res_x,f_f_sub,s,f,r,T)

for(b in 1:B){
  plotKDE(b,kde,res_x,f_f_sub,s,f,r,T)
}
