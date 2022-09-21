#' name <- "Aydin Ardalan , Seyed Mehdi Mirshojaei"
#' liuid <- "Aydar442, Seymi176"
#' github_username <- "aydinardalan, Seymi176"
#' Create a linear regression object
#' @param formula, The linear model's formula.
#' @param data, The provided dataset.
#' @field formula, Formula for linear model .
#' @field data, The linear model's formula.
#' @field X, matrix. The matrix of independent variables.
#' @field Y, matrix. The matrix of dependent variables.
#' @field Xt, transpose of m_X
#' @field XtX, inverse of matrix multiplication of m_X and X_t
#' @field B, matrix. Regressions coefficients.
#' @field Yfit, The fitted values for y.
#' @field E, The residuals.
#' @field sigsq, The residual variance. 
#' @field Estd, #The residual standard deviation. 
#' @field N, The number of observations in the model. 
#' @field P, The number of parameters in the model. 
#' @field var_B, The variance of the regression coefficients. 
#' @field tval, The t-values for each coefficient. 
#' @field pval, p-values computed according to t-vales and pt function. 
#' @field Formula, The exporting formula. 
#' @field Data, The exporting data.
#' @field sqrtstresiduals, The square root of standardized residuals.
#'
#' @return empty
#' @export linreg
#' @exportClass linreg
#' @import methods

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
                          tval <<- B/as.numeric(sqrt(diag(var_B)))
                          # Standardized residuals for summary plot
                          sqrtstresiduals <<- sqrt(abs(E/sd(E)))
                          # p-values
                          pval <<- 2 * pt(abs(tval), dof, lower.tail = FALSE)
                          #Saving names
                          Formula <<- formula
                          Data <<- deparse(substitute(data))
                        },
                        #Print function
                        print = function(){
                          cat(paste("linreg(formula = ", format(Formula), ", data = ", Data , ")\n\n ", sep = ""))
                          # setNames(round(B[1:nrow(B)],3),rownames(B))
                          cat(rownames(B), "\n")
                        },
                        resid = function(){
                          return(as.vector(E))
                        },
                        pred = function(){
                          return(Yfit)
                        },
                        coef = function(){
                          vec <- as.vector(B)
                          names(vec) <- colnames(X)
                          return(vec)
                        },
                        #Plot function
                        plot = function() {
                          "plots fitted values versus residuals and standardized residuals"
                          library(ggplot2)
                          plot1 <- ggplot(data.frame(Yfit, E), aes(y=E, x=Yfit)) + geom_point(shape=21, size=3, colour="black", fill="white")
                          plot1 <- plot1 
                          plot1 <- plot1 + stat_summary(fun.y=median, colour="red", geom="line", aes(group = 1))
                          plot1 <- plot1 + ggtitle("Residuals vs fitted") + xlab(paste("Fitted values \n lm(Petal.Length ~ Species)"))+ ylab("Residuals")
                          
                          plot2 <- ggplot(data.frame(Yfit, sqrtstresiduals), aes(y=sqrtstresiduals, Yfit)) + geom_point(alpha = 0.6, shape=21, size=3, colour="black", fill="white")
                          plot2 <- plot2 
                          plot2 <- plot2 + stat_summary(fun.y=median, colour="red", geom="line", aes(group = 1))
                          plot2 <- plot2 + ggtitle("Scale-Location") + xlab(paste("Fitted values \n lm(Petal.Length ~ Species)"))
                          plot2 <- plot2 + scale_x_continuous(breaks = seq(0.0, 1.5, by= 0.5))
                          
                          plotlist <- list(plot1, plot2)
                          return(plotlist)
                        },
                        
                        summary = function(){
                          cat(paste("linreg(formula = ", format(Formula), ", data = ", Data, ") :\n\n ", sep = ""))
                          x <- setNames(as.data.frame(cbind(B,as.matrix(sqrt(diag(var_B))),tval, formatC(pval, format = "e", digits = 2), count_stars_P(pval))), c("Coefficients","Standard error","t values", "p values", ""))
                          # y <- data.frame(rownames(B), x)
                          myPrint(x)
                          cat(paste("\n\nResidual standard error: ", Estd, " on ", dof, " degrees of freedom: ", sep = ""))
                        }
                      )
)

#' myPrint (custom print)
#' @param x object
#' @param stripoff stripoff column names will be stripped off.
#'
#' @return Nothing

myPrint = function(x, stripoff = FALSE) {
  print(x)
}

#' count_stars_P
#' It rates P-value with stars
#' @param pval The P_value
#'
#' @return 0 "***" 0.001 "**" 0.01 "*" 0.05 "." 0.1 " " 1

count_stars_P <- function(pval) {
  x<- ifelse(pval>0.1, " ",
             ifelse(pval>0.05, ".",
                    ifelse(pval>0.01, "*",
                           ifelse(pval>0.001, "**", "***"))))
  return(x)
}