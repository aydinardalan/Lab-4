data("iris")

#lm.ridge
mass_object_0 <- MASS::lm.ridge(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, data = iris,lambda=0)
lmridge0<- round(mass_object_0$coef,1)

mass_object_1 <- MASS::lm.ridge(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, data = iris,lambda=1)
lmridge1<-round(mass_object_1$coef,1)

mass_object_15 <- MASS::lm.ridge(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, data = iris,lambda=15)
lmridge15<-round(mass_object_15$coef,1)


#Ridgereg
ridgereg_object_0 <- ridgereg_QR(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, data = iris,lambda=0)
ridge0<-round(ridgereg_object_0$coef(),1)

ridgereg_object_1 <- ridgereg_QR(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, data = iris,lambda=1)
ridge1<-round(ridgereg_object_1$coef(),1)

ridgereg_object_15 <- ridgereg_QR(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, data = iris,lambda=15)
ridge15<-round(ridgereg_object_15$coef(),1)


test_that("Ridgereg function is giving the same outputs as lm.ridge from MASS package", {
  
  expect_equal(lmridge0,ridge0)
  expect_equal(lmridge1,ridge1)
  expect_equal(lmridge15,ridge15)
})

Polygon <- setRefClass("Polygon", fields = c("sides"))
square <- Polygon$new(sides = 4)
test_that("ridgereg rejects errounous input", {
  expect_error(ridgereg_mod <- ridgereg_QR$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=iris))
  expect_error(ridgereg_mod <- ridgereg_QR$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis))
})

test_that("class is correct", {
  ridgereg_mod <- ridgereg_QR$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  
  expect_true(class(ridgereg_mod)[1] == "ridgereg_QR")
})

test_that("predict() method works", {
  ridgereg_mod <- ridgereg_QR$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  
  expect_equal(round(unname(ridgereg_mod$predict()[c(1,5,7)]),2), c(1.85, 1.53, 1.09))    
})