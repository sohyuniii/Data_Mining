##### Exercise 9.4 ##### 
library(e1071)
set.seed(1)
x <- rnorm(100)
y <- 4 * x^2 + 1 + rnorm(100)
class <- sample(100, 50)
y[class] <- y[class] + 3
y[-class] <- y[-class] - 3
plot(x[class], y[class], col = "red", xlab = "X", ylab = "Y", ylim = c(-6, 30))
points(x[-class], y[-class], col = "blue")

z <- rep(-1, 100)
z[class] <- 1
data <- data.frame(x = x, y = y, z = as.factor(z))
train <- sample(100, 50)
data.train <- data[train, ]
data.test <- data[-train, ]
svm.linear <- svm(z ~ ., data = data.train, kernel = "linear", cost = 10)
plot(svm.linear, data.train)
table(predict = predict(svm.linear, data.train), truth = data.train$z)

svm.poly <- svm(z ~ ., data = data.train, kernel = "polynomial", cost = 10)
plot(svm.poly, data.train)
table(predict = predict(svm.poly, data.train), truth = data.train$z)

svm.radial <- svm(z ~ ., data = data.train, kernel = "radial", gamma = 1, cost = 10)
plot(svm.radial, data.train)
table(predict = predict(svm.radial, data.train), truth = data.train$z)

plot(svm.linear, data.test)
table(predict = predict(svm.linear, data.test), truth = data.test$z)
plot(svm.poly, data.test)
table(predict = predict(svm.poly, data.test), truth = data.test$z)
plot(svm.radial, data.test)
table(predict = predict(svm.radial, data.test), truth = data.test$z)

##### Exercise 9.5 #####
set.seed(1)
x1 <- runif(500) - 0.5
x2 <- runif(500) - 0.5
y <- 1 * (x1^2 - x2^2 > 0)

plot(x1, x2, xlab = "X1", ylab = "X2", col = (4 - y), pch = (3 - y))

logit.fit <- glm(y ~ x1 + x2, family = "binomial")
summary(logit.fit)

data <- data.frame(x1 = x1, x2 = x2, y = y)
probs <- predict(logit.fit, data, type = "response")
preds <- rep(0, 500)
preds[probs > 0.47] <- 1
plot(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1), xlab = "X1", ylab = "X2")
points(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0))

logitnl.fit <- glm(y ~ poly(x1, 2) + poly(x2, 2) + I(x1 * x2), family = "binomial")
summary(logitnl.fit)

probs <- predict(logitnl.fit, data, type = "response")
preds <- rep(0, 500)
preds[probs > 0.47] <- 1
plot(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1), xlab = "X1", ylab = "X2")
points(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0))

ata$y <- as.factor(data$y)
svm.fit <- svm(y ~ x1 + x2, data, kernel = "linear", cost = 0.01)
preds <- predict(svm.fit, data)
plot(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0), xlab = "X1", ylab = "X2")
points(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1))

data$y <- as.factor(data$y)
svmnl.fit <- svm(y ~ x1 + x2, data, kernel = "radial", gamma = 1)
preds <- predict(svmnl.fit, data)
plot(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0), xlab = "X1", ylab = "X2")
points(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1))

##### Exercise 9.8 #####
set.seed(1)
train <- sample(nrow(OJ), 800)
OJ.train <- OJ[train, ]
OJ.test <- OJ[-train, ]

svm.linear <- svm(Purchase ~ ., data = OJ.train, kernel = "linear", cost = 0.01)
summary(svm.linear)

train.pred <- predict(svm.linear, OJ.train)
table(OJ.train$Purchase, train.pred)
(78 + 55) / (439 + 228 + 78 + 55)
test.pred <- predict(svm.linear, OJ.test)
table(OJ.test$Purchase, test.pred)
(31 + 18) / (141 + 80 + 31 + 18)

set.seed(2)
tune.out <- tune(svm, Purchase ~ ., data = OJ.train, kernel = "linear", ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
summary(tune.out)

svm.linear <- svm(Purchase ~ ., kernel = "linear", data = OJ.train, cost = tune.out$best.parameter$cost)
train.pred <- predict(svm.linear, OJ.train)
table(OJ.train$Purchase, train.pred)
(71 + 56) / (438 + 235 + 71 + 56)
test.pred <- predict(svm.linear, OJ.test)
table(OJ.test$Purchase, test.pred)
(32 + 19) / (140 + 79 + 32 + 19)

svm.radial <- svm(Purchase ~ ., kernel = "radial", data = OJ.train)
summary(svm.radial)
train.pred <- predict(svm.radial, OJ.train)
table(OJ.train$Purchase, train.pred)
(77 + 39) / (455 + 229 + 77 + 39)
test.pred <- predict(svm.radial, OJ.test)
table(OJ.test$Purchase, test.pred)
(28 + 18) / (141 + 83 + 28 + 18)









