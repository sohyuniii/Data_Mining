##### Exercise 4.10 ##### 
require(ISLR)
data(Weekly)
library(psych)
pairs.panels(Weekly, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE) # show correlation ellipses)
fit.logit <- glm(Direction~., data=Weekly[,c(2:7,9)], family=binomial)
summary(fit.logit)
logit.prob <- predict(fit.logit, Weekly, type="response")
logit.pred <- ifelse(logit.prob > 0.5, "Up", "Down")
table(logit.pred, Weekly$Direction)
(54+557)/nrow(Weekly) 
train.yrs <- Weekly$Year %in% (1990:2008)
train <- Weekly[train.yrs,]
test <- Weekly[!train.yrs,]
fit2 <- glm(Direction~Lag2, data=train, family=binomial)
fit2.prob <- predict(fit2, test, type="response")
fit2.pred <- ifelse(fit2.prob > 0.5, "Up", "Down")
table(fit2.pred, test$Direction)
mean(fit2.pred == test$Direction)  # Accuracy=0.625

require(MASS)
fit.lda <- lda(Direction~Lag2, data=train)
fit.lda.pred <- predict(fit.lda, test)$class
table(fit.lda.pred, test$Direction)
mean(fit.lda.pred == test$Direction) 

fit.qda <- qda(Direction~Lag2, data=train)
fit.qda.pred <- predict(fit.qda, test)$class
table(fit.qda.pred, test$Direction)
mean(fit.qda.pred == test$Direction) 

require(class)
set.seed(1)
train.X <- as.matrix(train$Lag2)
test.X <- as.matrix(test$Lag2)
knn.pred <- knn(train.X, test.X, train$Direction, k=35)
table(knn.pred, test$Direction)
mean(knn.pred == test$Direction)  # Accuracy=0.500

acc <- rep(NA,10)
for (i in 1:10){
  k = 5*i
  knn.pred <- knn(train.X, test.X, train$Direction, k=5)
  acc[i] = mean(knn.pred == test$Direction)
}
plot(seq(5,50,5),acc,type="l",col=4, main="Accuracy for K in the KNN classifier")
acc[which.max(acc)]

fit.lda <- lda(Direction~Lag3+I(Lag3^1), data=train)
fit.lda <- lda(Direction~Lag2+Lag1+Lag3, data=train)
fit.lda.pred <- predict(fit.lda, test)$class
mean(fit.lda.pred == test$Direction)
table(fit.lda.pred, test$Direction)

##### Exercise 4.11 ##### 
require(ISLR)
data(Auto)
mpg01 <- ifelse(Auto$mpg > median(Auto$mpg), 1, 0)
table(mpg01)
mydf <- data.frame(Auto, mpg01)
pairs.panels(mydf, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE)

set.seed(1)
trainid <- sample(1:nrow(mydf), nrow(mydf)*0.7 , replace=F)  # 70% train, 30% test
train <- mydf[trainid,]
test <- mydf[-trainid,]

fit.lda <- lda(mpg01~displacement+horsepower+weight+acceleration, data=train)
fit.lda.pred <- predict(fit.lda, test)$class
table(fit.lda.pred, test$mpg01)
mean(fit.lda.pred != test$mpg01)  # error rate

fit.qda <- qda(mpg01~displacement+horsepower+weight+acceleration, data=train)
fit.qda.pred <- predict(fit.qda, test)$class
table(fit.qda.pred, test$mpg01)
mean(fit.qda.pred != test$mpg01)  # error rate

fit.logit <- glm(mpg01~displacement+horsepower+weight+acceleration, data=train, family=binomial)
logit.prob <- predict(fit.logit, test, type="response")
logit.pred <- ifelse(logit.prob > 0.5, 1, 0)
table(logit.pred, test$mpg01)
mean(logit.pred != test$mpg01)  # error rate

train.X <- cbind(train$displacement, train$horsepower, train$weight, train$acceleration)
test.X <- cbind(test$displacement, test$horsepower, test$weight, test$acceleration)

acc <- rep(NA,100)
for (i in 1:100){
  knn.pred <-knn(train.X, test.X, train$mpg01, k=i)
  acc[i] = mean(knn.pred != test$mpg01)
}
plot(acc,type="l",col=4, main="Error rate for K in the KNN classifier")
c(which.min(acc),acc[which.min(acc)])

knn.pred <- knn(train.X, test.X, train$mpg01, k=which.min(acc))
table(knn.pred, test$mpg01)
