### Exercise 9.7 ### 
library(ISLR)
set.seed(1)
dsc <- scale(USArrests)
d1 <- dist(dsc)^2
d2 <- as.dist(1 - cor(t(dsc)))
summary(d2 / d1) ; plot(d2 / d1)

### Exercise 9.10 ### 
set.seed(2)
x <- matrix(rnorm(20 * 3 * 50, mean = 0, sd = 0.001), ncol = 50)
x[1:20, 2] <- 1
x[21:40, 1] <- 2
x[21:40, 2] <- 2
x[41:60, 1] <- 1
true.labels <- c(rep(1, 20), rep(2, 20), rep(3, 20))

pr.out <- prcomp(x)
plot(pr.out$x[, 1:2], col = 1:3, xlab = "Z1", ylab = "Z2", pch = 19)

km.out <- kmeans(x, 3, nstart = 20)
table(true.labels, km.out$cluster)

km.out <- kmeans(x, 2, nstart = 20)
table(true.labels, km.out$cluster)

km.out <- kmeans(x, 4, nstart = 20)
table(true.labels, km.out$cluster)

km.out <- kmeans(pr.out$x[, 1:2], 3, nstart = 20)
table(true.labels, km.out$cluster)

km.out <- kmeans(scale(x), 3, nstart = 20)
table(true.labels, km.out$cluster)