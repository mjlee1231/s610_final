library(SuppDists)
library(mvtnorm)

draw_tau = function(data, state){
  shape = 1 + length(data$y)
  rate = 1 + sum(abs(data$y-data$x%*%state$beta))
  return(rgamma(1,shape,rate))
}

draw_beta = function(data, state){
  # Draw z vectors
  nu = 1/(state$tau*abs(data$y-data$x%*%state$beta))
  n = length(nu)
  z = sapply(nu, function (w) {rinvGauss(1, nu = w, lambda = 1)})
  Z = diag(z, nrow = n)
  
  # Draw a beta vector
  p = ncol(data$x)
  inv_mat = solve(state$tau^2*t(data$x)%*%Z%*%data$x + diag(p))
  mean = inv_mat%*%(state$tau^2*t(data$x))%*%Z%*%data$y

  return(as.vector(rmvnorm(1, mean = mean, sigma = inv_mat)))
}

update_current_state = function(data, state){
  state$beta = draw_beta(data,state)
  state$tau = draw_tau(data,state)
  return(state)
}

