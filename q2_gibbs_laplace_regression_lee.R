# setting project directory as working directory
setwd("/Users/jin/OneDrive - Indiana University/Fall 2021/S610/final/final_q2_files")

# sources header relative to the path for this file
source("headers/q2_header_lee.R")
load('final_q2_data.RData')

# gibbs sampling function
gibbs_laplace_regression = function(N_draws,x,y,beta_0,tau_0){
  # data manipulation
  if(is.null(colnames(x))){
    colnames(x) = paste("x",1:ncol(x),sep="_")
  }
  if(!all(abs(x[,1]-1)<.Machine$double.eps)){
    x = cbind(c(1),x)
    colnames(x)[1] = "intercept"
  }
  data = list(y=y,x=x)
  
  # checking for missing initial values and making them if necessary
  if(missing(beta_0)) beta_0 = c(median(data$y),rep(0,ncol(data$x)-1))
  if(missing(tau_0)) tau_0 = 1/mean(abs(data$y-data$x%*%beta_0))
  
  # initial state creation
  current_state = list(beta=beta_0,tau=tau_0)
  
  # output creation
  out = list(beta=matrix(NA,ncol(x),N_draws),tau=rep(NA,N_draws))
  rownames(out$beta) = colnames(x)
    
  # loop for sampling
  for(i in 1:N_draws){
    current_state = update_current_state(data,current_state)
    out$beta[,i] = current_state$beta
    out$tau[i] = current_state$tau
  }
  
  # returning
  return(out)
}


output = gibbs_laplace_regression(N_draws=1e4, x=x, y=y)
beta_mean = sapply(1:10, function(i) mean(output$beta[,i])) 
# 2.84502606  0.24851034  0.52695574 -0.43695430  0.69719037  0.09101311  0.43034238  0.12518303  0.74876705  1.11990374
tau_mean = mean(output$tau) # 0.006719281


library(gmodels)
library(Rmisc)
# Calculate CI for each beta
CI_beta = sapply(1:10, function(i) CI(output$beta[,i]))
CI_beta

#           [,1]       [,2]       [,3]       [,4]      [,5]        [,6]       [,7]       [,8]        [,9]     [,10]
# upper  9.194191  1.3065708  1.3720178  0.6079151 1.2591965  0.70649681  1.3606277  1.2229927  1.52832312 1.9790900
# mean   2.845026  0.2485103  0.5269557 -0.4369543 0.6971904  0.09101311  0.4303424  0.1251830  0.74876705 1.1199037
# lower -3.504139 -0.8095501 -0.3181063 -1.4818237 0.1351842 -0.52447059 -0.4999430 -0.9726266 -0.03078901 0.2607175

# I would say that beta_5 and beta_10 are non-zero.
