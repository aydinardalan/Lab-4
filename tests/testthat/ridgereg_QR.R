#'Ridge Regression with QR decomposition
#'
#'\code{ridgereg_QR} is a method of estimating the coefficients of multiple-regression models in scenarios where independent variables are highly correlated.
#'
#'\code{ridgereg_QR} differs from \code{ridgereg} in such a way that it performs QR decomposition as part of the linear system resolution.
#'
#'\code{ridgereg_QR} is an RC class. Upon object instantiation, in the \code{initialize} method, it performs
#'all calculations of quantities of interest, namely estimated coefficients and fitted dependent variable values,
#'
#'Predicted values and coefficients can be obtained through the methods \code{predict()} and \code{coef()}.
#'
#'\code{print()} prints out the input information used in the regression and the resulting coefficients.
#'
#'
#' @examples
#' data(iris)
#'regression_object <- ridgereg_QR(
#'   formula=Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length,
#'   data = iris, lambda=1)
#'regression_object$predict()
#'regression_object$coef()
#'regression_object$print()
#' 
#' @source 
#' Read more at \url{https://en.wikipedia.org/wiki/Ridge_regression}
#'
#' @export ridgereg_QR
#' @importFrom MASS lm.ridge

ridgereg_QR = setRefClass(
  Class = "ridgereg_QR",
  fields = c(  "coefs",
               "y_est",
               "local_data_name",
               "local_lambda"),
  methods = list(
    initialize = function(formula, data, lambda = 0){
      
      
      local_data_name <<- deparse(substitute(data))
      local_lambda<<- lambda
      
      my_data_x = model.matrix(object = formula, data = data)
      y = data[[all.vars(formula)[[1]]]]
      my_normalized_data = scale(my_data_x)
      X = as.matrix(my_normalized_data)[,-1]
      
      #Beginning of QR decomposition calculations
      
      Xupdated = rbind(X, diag(rep(sqrt(lambda), ncol(X))))
      yupdated = c(y, rep(0, ncol(X)))
      QR = qr(Xupdated)
      X_Q = qr.Q(QR)
      X_R = qr.R(QR)
      coefs <<- drop(solve(X_R, (t(X_Q)%*%yupdated)))
      
      #End of QR decomposition calculations
      
      beta_zero = mean(y)
      
      y_est <<- drop(X %*% coefs + beta_zero) #calculates estimated y
      
    },
    print = function(){
      cat(paste("ridgereg(formula = ", deparse(formula), ", data = ", local_data_name, ", lambda = ", local_lambda ,")\n\n", sep = ""))
      return(coefs)
    },
    predict = function(){
      return(y_est)
    },
    coef = function(){
      return(coefs)
    }
    
  )
)