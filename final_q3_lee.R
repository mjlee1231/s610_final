library(testthat)

eigen_fun = function(X,tol=.Machine$double.eps^0.75){
  if(!isSymmetric(X)) stop("x is not symmetric")
  d = ncol(X)
  U = diag(1,d,d,FALSE)
  while(TRUE){
    did_we_update = FALSE
    for(i in 1:(d-1)){
      for(j in (i+1):d){
        # test whether abs(X[i,j]) is too large
        if(abs(X[i,j])>tol){
          # if so, get the Givens Rotation matrix R
          # and update X and U
          if (X[i,i]==X[j,j]){
            theta = pi/4
          } else {
            theta = (1/2)*atan(2*X[j,i]/(X[i,i]-X[j,j]))
          }
          R = diag(1,d,d,FALSE)
          R[i,j] = sin(theta)
          R[j,i] = -sin(theta)
          R[i,i] = cos(theta)
          R[j,j] = cos(theta)
          U = U%*%t(R)
          X = R%*%X%*%t(R)
          
          # track that in this sweep through we have done at least one update
          did_we_update = TRUE
        }
        
      }
    }
    if(!did_we_update) break
  }
  values=diag(X)
  sorted_values = sort(values,index.return=TRUE,decreasing=TRUE)
  values = sorted_values$x
  vectors = U[,sorted_values$ix]
  return(list(values=values, vectors=vectors))
}

set.seed(1234567890)
z = sapply(2:5, function(d) matrix(rnorm(d^2),d,d))
X = sapply(z, function(u) u + t(u)) # Create symmetric matrices 2*2, 3*3, 4*4, 5*5
out1 = lapply(X, function (v) eigen_fun(v)) # output of eigen_fun
out2 = lapply(X, function (v) eigen(v)) # output of eigen

# Test of eigenvalues matching 
test_that('eigenvalues', {
  for (i in 1:4){
  expect_equal(out1[[i]]$values, out2[[i]]$values)
  }
})


# Function to switch the sign of eigenvectors
switch_sign = function (x,y) {
  for (i in 1:4){
    j = 1
    while (TRUE){
      if (all.equal(x[[i]]$vectors[,j], y[[i]]$vectors[,j], tolerance = sqrt(.Machine$double.eps)) == TRUE) {
        x[[i]]$vectors[,j] = x[[i]]$vectors[,j]
        j = j + 1
      } else {
        x[[i]]$vectors[,j] = (-1)*x[[i]]$vectors[,j]
        j = j + 1
      }
    if (j > dim(x[[i]]$vectors)[2]) break
    }
  }
  return(x)
}
# Output of sign switched function
sign_ch = switch_sign(out1, out2)

# Test of eigenvectors matching
test_that('eigenvectors',{
  for (i in 1:4){
    expect_equal(sign_ch[[i]]$vectors, out2[[i]]$vectors)
  }
})

