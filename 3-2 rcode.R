library(tidyverse)
library(psych)

####### Exercise 3.8 ####### 
### __ (a) __
library(ISLR)
data(Auto)
fit <- lm(mpg ~ horsepower, data=Auto) 
summary(fit)
new <- data.frame(horsepower = 98) 
predict(fit, new, interval = "confidence")  # conf interval 
predict(fit, new, interval = "prediction")  # pred interval 

### __ (b) __ 
par(mfrow=c(1,1))
plot(Auto$horsepower, Auto$mpg, xlab='horsepower',ylab='mpg',
     main='the least squares regression line') 
abline(fit, col="red") 

### __ (c) __ 
par(mfrow=c(2,2)) 
plot(fit) 
## residuals vs fitted plot shows that the relationship is non-linear 

####### Exercise 3.9 ####### 
### __ (a) __
pairs.panels(Auto, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
### __ (b) __
cor.matrix = cor(subset(Auto, select=-name)) 
write.csv(cor.matrix,"cor.csv")

### __ (c-d) __
fit.lm <- lm(mpg~.-name, data=Auto) 
summary(fit.lm) 
par(mfrow=c(2,2)) 
plot(fit.lm) 

### __ (e) __
fit.lm0 <- lm(mpg~displacement+weight+origin, data=Auto) 
fit.lm1 <- lm(mpg~displacement*weight, data=Auto) 
fit.lm2 <- lm(mpg~displacement:weight, data=Auto) 
summary(fit.lm0) 
summary(fit.lm1) 
summary(fit.lm2) 

### __ (f) __

fit.lm5 <- lm(mpg~displacement+I(log(weight)), data=Auto)
fit.lm4 <- lm(mpg~displacement+I(weight^(1/2)), data=Auto) 
fit.lm6 <- lm(mpg~displacement+I(weight^2), data=Auto) 
summary(fit.lm4) 
summary(fit.lm5) 
summary(fit.lm6) 

####### Exercise 3.13 ####### 
set.seed(1)
x <- rnorm(100)
eps <- rnorm(100,sd=0.25^0.5)
y <- -1 + 0.5*x + eps
length(y)
plot(x,y,main="Scatter plot of x and y")
fit.lm <- lm(y~x)
summary(fit.lm)

plot(x,y) 
abline(-1, 0.5, col="blue")  # true regression 
abline(fit.lm, col="red")    # fitted regression 
legend(x = c(0.5,2.5), y = c(-2.5,-1.8), 
      legend = c("population", "model fit"), 
      col = c("blue","red"), lwd=2 ) 

fit.lm1 <- lm(y~x+I(x^2)) 
summary(fit.lm1) 

eps2 <- rnorm(100,sd=1)
y2 <- -1 + 0.5*x + eps2
fit.lm3 <- lm(y2~x)
summary(fit.lm2)
plot(x,y) 
abline(-1, 0.5, col="blue")  # true regression 
abline(fit.lm2, col="red")    # fitted regression 
legend(x = c(0.5,2.5), y = c(-2.5,-1.7), 
       legend = c("population", "model fit"), 
       col = c("blue","red"), lwd=2 ) 

eps2 <- rnorm(100,sd=0.01)
y2 <- -1 + 0.5*x + eps2
fit.lm2 <- lm(y2~x)
summary(fit.lm2)
plot(x,y) 
abline(-1, 0.5, col="blue")  # true regression 
abline(fit.lm2, col="red")    # fitted regression 
legend(x = c(0.2,2.5), y = c(-2.52,-1.7), 
       legend = c("population", "model fit"), 
       col = c("blue","red"), lwd=2 ) 

interval = cbind(confint(fit.lm2),confint(fit.lm),confint(fit.lm3))

####### Exercise 3.14 ####### 
set.seed (1)
x1=runif (100)
x2 =0.5* x1+rnorm (100) /10
y=2+2* x1 +0.3* x2+rnorm (100)

cor(x1,x2)
plot(x1,x2)

fit.lm <- lm(y~x1+x2)
summary(fit.lm)

fit.lm2 <- lm(y~x1)
summary(fit.lm2)

fit.lm3 <- lm(y~x2)
summary(fit.lm3)

x1=c(x1 , 0.1)
x2=c(x2 , 0.8)
y=c(y,6)
fit.lm <- lm(y~x1+x2)
summary(fit.lm)
fit.lm2 <- lm(y~x1)
summary(fit.lm2)
fit.lm3 <- lm(y~x2)
summary(fit.lm3)

par(mfrow=c(2,2))
plot(fit.lm3)
