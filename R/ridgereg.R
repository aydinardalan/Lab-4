#' Create a ridge regression object
#' @param formula, The model's formula.
#' @param data, The provided dataset.
#' @field formula, Formula for ridge regression .
#' @field data, The provided dataset.
#' @field X, the normalized of the matrix of independent variables.
#' @field my_data_x, The matrix of independent variables.
#' @field y, The matrix of dependent variables.
#' @field yhat, The matrix of dependent variables.
#' @field BhatR, Regressions coefficients.
#' @field lambda, Penalty
#' @field data_name, The exporting data.
#' @export ridgereg
#' @exportClass ridgereg
#' @import methods
#' @source Read more at \url{https://en.wikipedia.org/wiki/Ridge_regression}

ridgereg <- setRefClass("ridgereg",
                      fields = list(formula="formula",
                                    data="data.frame",
                                    lambda="numeric",
                                    BhatR="matrix",
                                    yhat="matrix",
                                    data_name="character",
                                    X="matrix",
                                    my_data_x="matrix",
                                    y="matrix"),
                      
                      methods = list(
                        # Constructor (Creating object)
                        initialize=function(formula,data, lambda=0) {
                          my_data_x <<- model.matrix(formula,data)
                          y <<- as.matrix(data[all.vars(formula)[1]])
                          X <<- as.matrix(scale(my_data_x))[,-1]
                          BhatR <<- drop(solve((t(X)%*%X) + diag(rep(lambda, ncol(X)))) %*% (t(X)%*%y))
                          yhat <<- drop((X %*% BhatR) + mean(y))
                          data_name <<- deparse(substitute(data))
                        },
                        
                        #Print function
                        print = function(){
                          cat(paste("ridgereg(formula = ", format(formula), ", data = ", data_name, ", lambda = ", lambda , ")\n\n ", sep = ""))
                          # setNames(round(BhatR[1:nrow(BhatR)],3),rownames(BhatR))
                          cat(rownames(BhatR), "\n")
                        },
                        
                        predict = function(){
                          return(yhat)
                        },
                        coef = function(){
                          return(BhatR)
                        }
                      )
)