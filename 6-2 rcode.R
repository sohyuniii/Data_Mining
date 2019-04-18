library(ISLR)
library(glmnet)
library(leaps)
require(MASS)
library(pls)

##### EXERCISE 6.9 #####
data(College)
set.seed(1)
trainid <- sample(1:nrow(College), nrow(College)*0.7)
train <- College[trainid,]
test <- College[-trainid,]
str(College)

fit.lm <- lm(Apps~., data=train)
pred.lm <- predict(fit.lm, test)
(err.lm <- mean((test$Apps - pred.lm)^2))  # test error

xmat.train <- model.matrix(Apps~., data=train)[,-1]
xmat.test <- model.matrix(Apps~., data=test)[,-1]
fit.ridge <- cv.glmnet(xmat.train, train$Apps, alpha=0)
plot(fit.ridge)
(lambda <- fit.ridge$lambda.min)  # optimal lambda
pred.ridge <- predict(fit.ridge, s=lambda, newx=xmat.test)
(err.ridge <- mean((test$Apps - pred.ridge)^2))  # test error

xmat.train <- model.matrix(Apps~., data=train)[,-1]
xmat.test <- model.matrix(Apps~., data=test)[,-1]
fit.lasso <- cv.glmnet(xmat.train, train$Apps, alpha=1)
plot(fit.lasso)
lasso.mod=glmnet(xmat.train,train$Apps,alpha=1,lambda=10.3733)
plot(lasso.mod)
(lambda <- fit.lasso$lambda.min)  # optimal lambda
pred.lasso <- predict(fit.lasso, s=lambda, newx=xmat.test)
(err.lasso <- mean((test$Apps - pred.lasso)^2))  # test error
coef.lasso <- predict(fit.lasso, type="coefficients", s=lambda)[1:ncol(College),]
coef.lasso[coef.lasso != 0]
length(coef.lasso[coef.lasso != 0])

set.seed(1)
fit.pcr <- pcr(Apps~., data=train, scale=TRUE, validation="CV")
validationplot(fit.pcr, val.type="MSEP")
summary(fit.pcr)
pred.pcr <- predict(fit.pcr, test, ncomp=16)  # min Cv at M=16
(err.pcr <- mean((test$Apps - pred.pcr)^2))  # test error

set.seed(1)
fit.pls <- plsr(Apps~., data=train, scale=TRUE, validation="CV")
validationplot(fit.pls, val.type="MSEP")
summary(fit.pls)
pred.pls <- predict(fit.pls, test, ncomp=10)  # min Cv at M=10
(err.pls <- mean((test$Apps - pred.pls)^2))  # test error

err.all <- c(err.lm, err.ridge, err.lasso, err.pcr, err.pls)
names(err.all) <- c("lm", "ridge", "lasso", "pcr", "pls")
barplot(err.all )

plot(test$Apps, pred.lm)


##### EXERCISE 6.11 ##### 
data(Boston)
set.seed(1)
trainid <- sample(1:nrow(Boston), nrow(Boston)/2)
train <- Boston[trainid,]
test <- Boston[-trainid,]
xmat.train <- model.matrix(crim~., data=train)[,-1]
xmat.test <- model.matrix(crim~., data=test)[,-1]
str(Boston)

# ridge regression model
fit.ridge <- cv.glmnet(xmat.train, train$crim, alpha=0)
(lambda <- fit.ridge$lambda.min)  # optimal lambda
pred.ridge <- predict(fit.ridge, s=lambda, newx=xmat.test)
(err.ridge <- mean((test$crim - pred.ridge)^2))  # test error
predict(fit.ridge, s=lambda, type="coefficients")

# lasso regression model
fit.lasso <- cv.glmnet(xmat.train, train$crim, alpha=1)
(lambda <- fit.lasso$lambda.min)  # optimal lambda
pred.lasso <- predict(fit.lasso, s=lambda, newx=xmat.test)
(err.lasso <- mean((test$crim - pred.lasso)^2))  # test error
predict(fit.lasso, s=lambda, type="coefficients")

# predict function from chapter 6 labs
predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

# forward selection
fit.fwd <- regsubsets(crim~., data=train, nvmax=ncol(Boston)-1)
(fwd.summary <- summary(fit.fwd))
err.fwd <- rep(NA, ncol(Boston)-1)
for(i in 1:(ncol(Boston)-1)) {
  pred.fwd <- predict(fit.fwd, test, id=i)
  err.fwd[i] <- mean((test$crim - pred.fwd)^2)
}
plot(err.fwd, type="b", main="Test MSE for Forward Selection", xlab="Number of Predictors")
which.min(err.fwd)

# backward selection
fit.bwd <- regsubsets(crim~., data=train, nvmax=ncol(Boston)-1)
(bwd.summary <- summary(fit.bwd))
err.bwd <- rep(NA, ncol(Boston)-1)
for(i in 1:(ncol(Boston)-1)) {
  pred.bwd <- predict(fit.bwd, test, id=i)
  err.bwd[i] <- mean((test$crim - pred.bwd)^2)
}
plot(err.bwd, type="b", main="Test MSE for Backward Selection", xlab="Number of Predictors")
which.min(err.bwd)

par(mfrow=c(3,2))

min.cp <- which.min(fwd.summary$cp)  
plot(fwd.summary$cp, xlab="Number of Poly(X)", ylab="Forward Selection Cp", type="l")
points(min.cp, fwd.summary$cp[min.cp], col="red", pch=4, lwd=5)

min.cp <- which.min(bwd.summary$cp)  
plot(bwd.summary$cp, xlab="Number of Poly(X)", ylab="Backward Selection Cp", type="l")
points(min.cp, bwd.summary$cp[min.cp], col="red", pch=4, lwd=5)

min.bic <- which.min(fwd.summary$bic)  
plot(fwd.summary$bic, xlab="Number of Poly(X)", ylab="Forward Selection BIC", type="l")
points(min.bic, fwd.summary$bic[min.bic], col="red", pch=4, lwd=5)

min.bic <- which.min(bwd.summary$bic)  
plot(bwd.summary$bic, xlab="Number of Poly(X)", ylab="Backward Selection BIC", type="l")
points(min.bic, bwd.summary$bic[min.bic], col="red", pch=4, lwd=5)

min.adjr2 <- which.max(fwd.summary$adjr2)  
plot(fwd.summary$adjr2, xlab="Number of Poly(X)", ylab="Forward Selection Adjusted R^2", type="l")
points(min.adjr2, fwd.summary$adjr2[min.adjr2], col="red", pch=4, lwd=5)

min.adjr2 <- which.max(bwd.summary$adjr2)  
plot(bwd.summary$adjr2, xlab="Number of Poly(X)", ylab="Backward Selection Adjusted R^2", type="l")
points(min.adjr2, bwd.summary$adjr2[min.adjr2], col="red", pch=4, lwd=5)

err.ridge
err.lasso
err.fwd
err.bwd