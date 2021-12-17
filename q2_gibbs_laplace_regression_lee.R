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

set.seed(1234567890)
output = gibbs_laplace_regression(N_draws=1e4, x=x, y=y)
beta_mean = sapply(1:10, function(i) mean(output$beta[,i])) 
# 6.71127592  0.91486425  0.44484356 -0.08133145  0.59614336  0.71693246  0.53092687  0.21546839 -0.11159513 -0.03081609
tau_mean = mean(output$tau) # 0.006720713


library(gmodels)
library(Rmisc)
# Calculate CI for each beta
CI_beta = sapply(1:10, function(i) CI(output$beta[,i]))
CI_beta

#            [,1]       [,2]       [,3]        [,4]       [,5]       [,6]       [,7]       [,8]       [,9]       [,10]
# upper 18.236863  2.9128177  1.5167453  0.70527636  1.3127631  1.9155295  1.6431464  0.8946899  1.2634366  1.15775950
# mean   6.711276  0.9148643  0.4448436 -0.08133145  0.5961434  0.7169325  0.5309269  0.2154684 -0.1115951 -0.03081609
# lower -4.814311 -1.0830892 -0.6270582 -0.86793927 -0.1204764 -0.4816645 -0.5812926 -0.4637532 -1.4866268 -1.21939168

# There is no beta which I can confidently say is non-zero as all confidence intervals include zero.
