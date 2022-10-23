#'Ridge Regression with QR decomposition

#' @importFrom MASS lm.ridge

ridgereg_QR = setRefClass("ridgereg_QR",
  fields = list(coefs="numeric",
                y_est="numeric",
                local_data_name="character",
                local_lambda="numeric"),
  
  methods = list(
    initialize = function(formula, data, lambda = 0){
      
      local_data_name <<- deparse(substitute(data))
      local_lambda<<- lambda
      
      my_data_x = model.matrix(object = formula, data = data)
      y = data[[all.vars(formula)[[1]]]]
      my_normalized_data = scale(my_data_x)
      X = as.matrix(my_normalized_data)[,-1]
      
      # QR Decomposition
      Xupdated = rbind(X, diag(rep(sqrt(lambda), ncol(X))))
      yupdated = c(y, rep(0, ncol(X)))
      QR = qr(Xupdated)
      X_Q = qr.Q(QR)
      X_R = qr.R(QR)
      coefs <<- drop(solve(X_R, (t(X_Q)%*%yupdated)))
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