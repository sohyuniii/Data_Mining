
### generate p-dim unit sphere random points ###
library(dplyr)
library(mvtnorm)
fun.generate.pts <- function(n,p){
  
  # n : # of observations
  # p : number of dimensions
  
  x <- rmvnorm(n, mean = rep(0,p), sigma = diag(p))
  x <- t(apply(x, 1, function(z){runif(1)^(1/p)*z/sqrt(sum(z^2))}))
  return(x)

}

dim2 <- fun.generate.pts(10000,2)
sum(apply(dim2,1,function(z){sqrt(sum(z^2))}) > 1 )
plot(dim2[,1],dim2[,2],xlab='x',ylab='y',main="p=2 dim unit sphere",col="red")

library(plot3D)
dim3 <- fun.generate.pts(10000,3)
sum(apply(dim3,1,function(z){sqrt(sum(z^2))}) > 1 )
x <- dim3[,1]
y <- dim3[,2]
z <- dim3[,3]
scatter3D(x, y, z, pch = 18,  theta = 20, phi = 20,
          main = "p=3 dim unit sphere", 
          xlab = "x", ylab ="y", zlab = "z")


### compare actual value & simulation value ###

fun.distance <- function(n,p){
  (1-0.5^(1/n))^(1/p)
}

fun.simul <- function(n, p, sim.n){
  
  min.d <- rep(NA,sim.n)
  
  for (i in 1:sim.n){
    data <- fun.generate.pts(n,p)
    d <- sqrt( rowSums(data^2) )
    min.d[i] <- min(d)
  }
  m <- median(min.d)
  return(m)
  
}

### (1) n, sim.n -> fix , p change ###
n = 100 ;  p = 30 ; sim.n = 1000

actual.m <- rep(NA,p)
for (i in 2:p){
  actual.m[i] = fun.distance(n,i)
}

sim.m <- rep(NA,p)
for (i in 2:p){
  sim.m[i] = fun.simul(n,i,sim.n)
}

compare1 <- data.frame(actual.m, sim.m)
compare1 <- compare1 %>% mutate(res = actual.m-sim.m )
mean(abs(compare1$res),na.rm = T)
plot(abs(compare1$res),type='l')

### (2) n,p -> fix , sim.n change ###

n = 100 ;  p = 5 ; sim.n = 1000

actual.m <- rep(fun.distance(n,p),10)

sim.m <- rep(NA,10)
for (sim.n in 1:10){
  sim.m[sim.n] = fun.simul(n,p,sim.n*100)
}

compare2 <- data.frame(actual.m, sim.m)
compare2 <- compare2 %>% mutate(res = actual.m-sim.m )
mean(abs(compare2$res))
plot(abs(compare2$res),type='l',xlab='number of simulation',ylab='error',
     main='Simulate with N=100, p=5')

n = 1000 ;  p = 5 ; sim.n = 1000

actual.m <- rep(fun.distance(n,p),10)

sim.m <- rep(NA,10)
for (sim.n in 1:10){
  sim.m[sim.n] = fun.simul(n,p,sim.n*100)
}

compare3 <- data.frame(actual.m, sim.m)
compare3 <- compare3 %>% mutate(res = actual.m-sim.m )
mean(abs(compare3$res))
plot(abs(compare3$res),type='l',xlab='number of simulation',ylab='error',
     main='Simulate with N=1000, p=5')
