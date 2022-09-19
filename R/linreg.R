linreg <- setRefClass("linreg",
                      fields = list(formula="formula",
                                    data="data.frame",
                                    X="matrix",
                                    Y="matrix",
                                    Xt="matrix",
                                    XtX="matrix",
                                    B="matrix",
                                    Yfit="matrix",
                                    E="matrix",
                                    dof="integer",
                                    sigsq="numeric",
                                    Estd="numeric",
                                    N="numeric",
                                    P="numeric",
                                    var_B="matrix",
                                    tval="matrix",
                                    pval="matrix",
                                    Formula="formula",
                                    Data="character",
                                    sqrtstresiduals="matrix"),
                      
                      methods = list(
                        # Constructor (Creating object)
                        initialize=function(formula,data) {
                          # Dependent and independent variables
                          X <<- model.matrix(formula,data)
                          Y <<- as.matrix(data[all.vars(formula)[1]])
                          # Regressions Coefficients
                          B <<- solve(t(X)%*% X)%*% t(X) %*% Y
                          # Fitted values
                          Yfit <<- X %*% B
                          # Residuals
                          E <<- Y-Yfit
                          P <<- length(B)
                          N <<- length(Y)
                          # Degrees of freedom
                          dof <<- N-P
                          # The Residual Variance
                          sigsq <<- as.vector(t(E)%*%E/dof)                                                  
                          Estd <<- sqrt(sigsq) 
                          # Variance of Regression Coefficients
                          Xt <<- t(X)
                          XtX <<- solve(Xt %*% X)
                          var_B <<- sigsq * XtX   
                          tval <<- B/as.numeric(sqrt(var(B)))
                          # Standardized residuals for summary plot
                          sqrtstresiduals <<- sqrt(abs(E/sd(E)))
                          # p-values
                          pval <<- 2 * pt(abs(tval), dof, lower.tail = FALSE)
                          #Saving names
                          Formula <<- formula
                          Data <<- deparse(substitute(data))
                        }
)
)