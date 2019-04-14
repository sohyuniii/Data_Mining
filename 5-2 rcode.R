##### Exercise 5.2 ##### 
plot(1:10000, 1-(1-1/1:10000)^(1:10000),type='b')

store <- rep(NA, 10000)
for (i in 1:10000)
  store[i] <- sum(sample(1:100, rep=TRUE)==4) > 0
mean(store)

##### Exercise 5.5 ##### 
require(ISLR)
data(Default)
set.seed(1)
fit1 <- glm(default ~ income + balance, data=Default, family=binomial)
summary(fit1)

set.seed(1234)
train <- sample(nrow(Default), nrow(Default)*0.7)
fit2 <- glm(default ~ income + balance, data=Default, family=binomial, subset=train)
prob2 <- predict(fit2, Default[-train,], type="response")
pred2 <- ifelse(prob2 > 0.5, "Yes", "No")
table(pred2, Default[-train,]$default)
mean(Default[-train,]$default != pred2)  # test error

set.seed(56)
train <- sample(nrow(Default), nrow(Default)*0.7)
fit2 <- glm(default ~ income + balance, data=Default, family=binomial, subset=train)
prob2 <- predict(fit2, Default[-train,], type="response")
pred2 <- ifelse(prob2 > 0.5, "Yes", "No")
table(pred2, Default[-train,]$default)
mean(Default[-train,]$default != pred2)  # test error

set.seed(789)
train <- sample(nrow(Default), nrow(Default)*0.7)
fit2 <- glm(default ~ income + balance, data=Default, family=binomial, subset=train)
prob2 <- predict(fit2, Default[-train,], type="response")
pred2 <- ifelse(prob2 > 0.5, "Yes", "No")
table(pred2, Default[-train,]$default)
mean(Default[-train,]$default != pred2)  # test error

set.seed(56789)
train <- sample(nrow(Default), nrow(Default)*0.7)
fit2 <- glm(default ~ income + balance, data=Default, family=binomial, subset=train)
prob2 <- predict(fit2, Default[-train,], type="response")
pred2 <- ifelse(prob2 > 0.5, "Yes", "No")
table(pred2, Default[-train,]$default)
mean(Default[-train,]$default != pred2)  # test error

set.seed(1)
train <- sample(nrow(Default), nrow(Default)*0.7)
fit3 <- glm(default ~ income + balance + student, data=Default, family=binomial, subset=train)
prob3 <- predict(fit3, Default[-train,], type="response")
pred3 <- ifelse(prob3 > 0.5, "Yes", "No")
table(pred3, Default[-train,]$default)
mean(Default[-train,]$default != pred3)  # test error

##### Exercise 5.7 #####
require(ISLR)
data(Weekly)
set.seed(1)
fit1 <- glm(Direction ~ Lag1 + Lag2, data=Weekly, family=binomial)
summary(fit1)

set.seed(1)
fit2 <- glm(Direction ~ Lag1 + Lag2, data=Weekly, family=binomial, subset=2:nrow(Weekly))
summary(fit2)

ifelse(predict(fit2, Weekly[1,], type="response")>0.5, "Up", "Down")
Weekly[1,]$Direction

set.seed(1)
loocv.err <- rep(0,nrow(Weekly))
for (i in 1:nrow(Weekly)) {
  myfit <- glm(Direction ~ Lag1 + Lag2, data=Weekly[-i,], family=binomial)
  mypred <- ifelse(predict(myfit, Weekly[1,], type="response")>0.5, "Up", "Down")
  loocv.err[i] <- ifelse(Weekly[i,]$Direction==mypred, 0, 1)
}
table(loocv.err)
mean(loocv.err)

##### Exercise 5.9 #####
require(MASS)
require(boot)
data(Boston)
(medv.mu <- mean(Boston$medv))
(medv.sd <- sd(Boston$medv)/sqrt(nrow(Boston)))
set.seed(1)
mean.fn <- function(var, id) {
  return(mean(var[id]))
}
(boot.res <- boot(Boston$medv, mean.fn, R=100))

boot.res$t0 - 2*sd(boot.res$t)  # lower bound
boot.res$t0 + 2*sd(boot.res$t)  # upper bound
t.test(Boston$medv)

(medv.median <- median(Boston$medv))

set.seed(1)
median.fn <- function(var, id) {
  return(median(var[id]))
}
(boot.res <- boot(Boston$medv, median.fn, R=100))