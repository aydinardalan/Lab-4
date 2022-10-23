#'Ridge Regression with QR decomposition

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
    }
  )
)