#' Create a ridge regression object
#'
#' @field formula The model's formula. 
#' @field data The provided dataset. 
#' @field lambda penalty. 
#' @field BhatR Coefficients. 
#' @field yhat Estimated values of y. 
#' @field data_name The name of the data frame. 
#' @field X The matrix of independent variables. 
#' @field my_data_x The imported data frame. 
#' @field y The matrix of dependent variables. 
#'
#' @param formula, The model's formula.
#' @param data, The provided dataset.
#' @export ridgereg
#' @exportClass ridgereg
#' @import methods
#' @source Read more at \url{https://en.wikipedia.org/wiki/Ridge_regression}
#' @importFrom MASS lm.ridge

ridgereg <- setRefClass("ridgereg",
                      fields = list(formula="formula",
                                    data="data.frame",
                                    lambda="numeric",
                                    BhatR="numeric",
                                    yhat="numeric",
                                    data_name="character",
                                    X="matrix",
                                    my_data_x="matrix",
                                    y="matrix",
                                    Formula="formula",
                                    local_lambda="numeric"),
                      
                      methods = list(
                        # Constructor (Creating object)
                        initialize=function(formula,data, lambda=0) {
                          my_data_x <<- model.matrix(formula,data)
                          y <<- as.matrix(data[all.vars(formula)[1]])
                          X <<- as.matrix(scale(my_data_x))[,-1]
                          BhatR <<- drop(solve((t(X)%*%X) + diag(rep(lambda, ncol(X)))) %*% (t(X)%*%y))
                          yhat <<- drop((X %*% BhatR)+  mean(y))
                          Formula <<- formula
                          data_name <<- deparse(substitute(data))
                          local_lambda <<- lambda
                        },
                        
                        #Print function
                        print = function(){
                          cat(paste("ridgereg(formula = ", format(Formula), ", data = ", data_name, ", lambda = ", local_lambda , ")\n\n ", sep = ""))
                          # setNames(round(BhatR[1:nrow(BhatR)],3),rownames(BhatR))
                          cat(rownames(BhatR), "\n")
                          BhatR
                        },
                        
                        predict = function(){
                          return(yhat)
                        },
                        coef = function(){
                          return(BhatR)
                        }
                      )
)