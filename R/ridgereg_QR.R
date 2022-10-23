formula=Petal.Length~Species
data=iris
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