#' @keywords internal
"_PACKAGE"

linreg <- function(var,data){
  
  #Formula
  X <<- model.matrix(var, data)
  y <<- as.matrix(data[all.vars(var)[1]])
  Beta <<- solve(t(X)%*%X)%*%t(X)%*%y
  y_fitted <<- X%*%Beta
  e <<- y - y_fitted
  n <<- length(y)
  p <<- length(all.vars(var))
  df <<- n - p -1
  sigma_square <<- (t(e) %*% e)/df
  var_B <<- as.numeric(sigma_square) * solve(t(X)%*%X)
  t_value <<- Beta / as.numeric(sqrt(var(Beta)))
}


## usethis namespace: start
## usethis namespace: end
NULL
