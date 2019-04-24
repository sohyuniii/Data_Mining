library(ISLR)
data(Auto)

####### NO 9 #######

### (a) ###
# quantitative: mpg, cylinders, displacement, horsepower, weight, acceleration, year_
# qualitative: origin, name

### (b) ###
sapply(Auto[,1:7], range)

### (c) ###
sapply(Auto[,1:7], mean)
sapply(Auto[,1:7], sd)

### (d) ###
# create temp matrix for numeric columns
tmp <- Auto[,-(8:9)]   # drop origin, name
tmp <- tmp[-(10:85),]  # drop rows
r = sapply(tmp, range)
m = sapply(tmp, mean)
sd = sapply(tmp, sd)
data.frame(r[1,],r[2,],m,sd)

rr = sapply(Auto[,1:7], range)
data.frame(r[1,],rr[1,],r[2,],rr[2,])

### (e) ###
library(psych)
pairs.panels(Auto[,1:7], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

### (f) ###
model1 <- lm(data=tmp,mpg~.)
summary(model1)
model2 <- step(model1)
summary(model2)

####### NO 10 #######
  
### (a) ###
require(MASS)
data(Boston)
dim(Boston)

### (b) ###
pairs.panels(Boston, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
bosmelt <- melt(Boston, id="dis")
ggplot(bosmelt, aes(x=value, y=dis)) +
  facet_wrap(~variable, scales="free") + geom_point()

### (c) ###
require(ggplot2)
require(reshape2)
# plot each feature against crim rate
bosmelt <- melt(Boston, id="crim")
ggplot(bosmelt, aes(x=value, y=crim)) +
facet_wrap(~variable, scales="free") + geom_point()

### (d) ###
ggplot(Boston, aes(crim,fill=2)) + ylab("") + ggtitle("Density of Crime") +
  geom_density(alpha = 0.2, na.rm = TRUE) 
ggplot(Boston, aes(tax,fill=2)) + ylab("") + ggtitle("Density of Tax rates") +
  geom_density(alpha = 0.2, na.rm = TRUE) 
ggplot(Boston, aes(ptratio,fill=2)) + ylab("") + ggtitle("Density of Pupil-teacher ratios") +
  geom_density(alpha = 0.2, na.rm = TRUE) 

### (e) ###
table(Boston$chas)  # 35 towns

### (f) ###
median(Boston$ptratio)  # 19.05

### (h) ###
# there are two towns with lowest medv value of 5
Boston[Boston$medv == min(Boston$medv),]
# overall quartiles and range of predictors
qua = sapply(Boston, quantile)

### (h) ###
# count of towns
nrow(Boston[Boston$rm > 7,])  
nrow(Boston[Boston$rm > 8,])  

compare <- rbind(sapply(Boston[Boston$rm > 8,], mean), 
      sapply(Boston[Boston$rm <= 8,], mean))



