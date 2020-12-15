## -----------------------------------------------------------------------------
x <- rnorm(10)
y <- rnorm(10)
opar <- par()
par(bg="lightyellow", col.axis="blue", mar=c(4, 4, 2.5, 0.25))
plot(x, y, xlab="Ten random values", ylab="Ten other values",
xlim=c(-2, 2), ylim=c(-2, 2), pch=22, col="red", bg="yellow",
bty="l", tcl=-.25, las=1, cex=1.5)
title("A simple plot", font.main=3, adj=1)

## -----------------------------------------------------------------------------
library(vcd)
b<-table(Arthritis$Sex,Arthritis$Improved)
addmargins(b)

## -----------------------------------------------------------------------------
n <- 10000
u <- runif(n)
x <-2/(sqrt(1-u)) # inverse transformation
hist(x[x>0 & x<20], freq = FALSE, breaks = seq(0,20,0.5), main = "Histogram of the Pareto sample", xlab = "value")    # graph the density histogram
f <- function(x) {8/x^3}    # true pdf
curve(f, 2, 20, col = 2, add = TRUE)    # add the true density curve
legend(12,0.6,"true density", col = 2, lwd = 1)    # add a legend


## -----------------------------------------------------------------------------
n <- 1e4;
k<-0;
y <- numeric(n)
while (k < n) {
  u1 <- runif(1,-1,1)
  u2 <- runif(1,-1,1)
  u3 <- runif(1,-1,1)
  if ( abs(u3)>= abs(u2) & abs(u3)>=abs(u1)) {
    x <- u2    
    }else{
      x <- u3
      }
  k <- k + 1
  y[k] <- x
}

hist(y, freq = FALSE, breaks = seq(-1,1,0.01), main = "Histogram with the density curve", xlab = "value")
f <- function(x) {3/4*(1-x^2)}
curve(f, -1, 1, col = 2, add = TRUE)   
legend(0.5,0.85,"true density", col = 2, lwd = 1, cex=0.6)    # add a legend

## -----------------------------------------------------------------------------
n <- 1000
u <- runif(n)
x <-2*(1-(1-u)^(1/4))/((1-u)^(1/4))
hist(x, prob = TRUE, main = expression(f(x)==64/(2+x)^5))
y <- seq(0, 10, .01)
lines(y, 64/(2+y)^5)

## -----------------------------------------------------------------------------
m <- 1e4; 
x <- runif(m, min=0, max=pi/3)
theta.hat <- mean(sin(x)) * pi/3
print(c(theta.hat,cos(0) - cos(pi/3)))

## -----------------------------------------------------------------------------
m1 <-1e6
u1 <- runif(m1,0,1)
g1 = exp(u1) #Estimate theta by the simple Monte Carlo method
m2 <- 5e5
u2 <- runif(m2,0,1)
g2 = (exp(u2)+exp(1-u2))/2 #Estimate theta by the antithetic variate approach
c(1-var(g2) / var(g1))

## -----------------------------------------------------------------------------
m <- 10000
theta.hat <- se <- numeric(2)
g <- function(x) {
  x^2 * exp(-(x^2)/2)/sqrt(2*pi) * (x > 1) 
}

x <- rgamma(m,3,2) #using f1
fg <- g(x) / dgamma(x,3,2)
theta.hat[1] <- mean(fg)
se[1] <- sd(fg)

x <- rweibull(m,2,sqrt(2)) #using f2
fg <- g(x) / dweibull(x,2,sqrt(2))
theta.hat[2] <- mean(fg)
se[2] <- sd(fg)

theta.hat
se
rbind(theta.hat,se)

## -----------------------------------------------------------------------------
t<-seq(1,10,0.1)
g<-exp(-t^2/2)*t^2/sqrt(2*pi)
f1<-dgamma(t,3,2)
f2<-dweibull(t,2,sqrt(2))
 
####get the curve of g,f1 and f2####
plot(t,g,type="l",col="black",main="compare g(t), f1(t) and f2(t) ")   
lines(t,f1,col="red")  
lines(t,f2,col="green")  
legend("topright",legend =c('g(t)','f1(t)',"f2(t)") ,lty=1,col=c("black","red","green")) 


## -----------------------------------------------------------------------------
t<-seq(1,10,0.1)
 g<-exp(-t^2/2)*t^2/sqrt(2*pi)
 f2<-dweibull(t,2,sqrt(2))
 f1<-dgamma(t,3,2)
r1<-g/f1
r2<-g/f2
plot(t,r2,col="red", type = "l")
lines(t,r1,col="green")
title(main="ratio function")

## -----------------------------------------------------------------------------
M <- 10000 #number of replicates
k <- 5 #number of strata
r <- M / k #replicates per stratum
N <- 50 #number of times to repeat the estimation
T2 <- numeric(k)
estimates <- matrix(0, N, 2)
g <- function(x) {
exp(-x - log(1+x^2)) * (x > 0) * (x < 1)
}
u <- runif(M) #f3, inverse transform method
x <- - log(1 - u * (1 - exp(-1)))
fg <- g(x) / (exp(-x) / (1 - exp(-1)))
for (i in 1:N) {
  u <- runif(M) #f3, inverse transform method
  x <- - log(1 - u * (1 - exp(-1)))
  fg <- g(x) / (exp(-x) / (1 - exp(-1)))
  estimates[i, 1] <- mean(fg)
  for (j in 1:k){
    u <- runif(M/k, (j-1)/k, j/k)
    x <- - log(1 - u * (1 - exp(-1)))
    fg <- g(x) / (exp(-x) / (1 - exp(-1)))
    T2[j] <- mean(fg)}
  estimates[i, 2] <- mean(T2)
}
apply(estimates, 2, mean)
apply(estimates, 2, var)

## -----------------------------------------------------------------------------
n <- 20
alpha <- .05
UCL <- replicate(1000, expr = {
x <- rnorm(n, mean = 1, sd = 1)
s <- sum((x - mean(x))^2) / (n-1)
(3/2-mean(x)-s/2)/ sqrt(s*(1+s/2)/n)
} )
#count the number of intervals that contain sigma^2=4
sum(abs(UCL)<=2.093)
#or compute the mean to get the confidence level
mean(abs(UCL)<=2.093)

## -----------------------------------------------------------------------------
set.seed(1)

n <- 20
alpha <- 0.05
m <- 1000
UCLvar1 <- UCLmean1 <- UCLvar2 <- UCLmean2 <- numeric(m)
UCLvar <- UCLmean <- numeric(2)

UCLvar1 <- replicate(1000, expr = {
x <- rnorm(n, mean = 0, sd = 2)
(n-1) * var(x) / qchisq(alpha, df = n-1) })
UCLvar[1] <- mean(UCLvar1 > 4)

UCLvar2 <- replicate(1000, expr = {
x <- rchisq(n, df = 2)
(n-1) * var(x) / qchisq(alpha, df = n-1)
} )
UCLvar[2] <- mean(UCLvar2 > 4)

UCLmean1 <- replicate(1000,expr={
  y <- rnorm(n, mean = 0, sd = 2)
  (mean(y)-sd(y)*qt(df=n-1,alpha)/sqrt(n))
  
})
UCLmean[1] <- mean(UCLmean1 > 0)

UCLmean2 <- replicate(1000,expr={
  y <- rchisq(n,df=2)
  (mean(y)-sd(y)*qt(df=n-1,alpha)/sqrt(n))
  
})
UCLmean[2] <- mean(UCLmean2 > 2)

f <-data.frame(UCLmean,UCLvar,row.names = c("normal distribution","chi-square distribution"))
knitr::kable(f)

## -----------------------------------------------------------------------------
sk <- function(x) {
#computes the sample skewness coeff.
xbar <- mean(x)
m3 <- mean((x - xbar)^3)
m2 <- mean((x - xbar)^2)
return( m3 / m2^1.5 )
}

# beta(a,a)
pwr_beta = function(a){
 alpha = 0.1
 n = 20
 m = 1e4
 N = length(a)
 pwr = numeric(N)
 cv = qnorm(1-alpha/2, 0, sqrt(6*(n-2) / ((n+1)*(n+3))))
 
 for (j in 1:N) { 
  sktests = numeric(m)
  for (i in 1:m) { 
   x = rbeta(n, a[j], a[j])
   sktests[i] = as.integer(abs(sk(x))>= cv)
  }
  pwr[j] = mean(sktests)
 }
 se = sqrt(pwr * (1-pwr) / m) 
 return(list(pwr = pwr,se = se))
}

 a = c(seq(0,1,0.1),seq(1,20,1),seq(20,100,10))
 pwr = pwr_beta(a)$pwr
 # plot the power
 se = pwr_beta(a)$se
 plot(a, pwr, type = "b", xlab = "a", ylab = "pwr", pch=16)
 abline(h = 0.1, lty = 2)
 lines(a, pwr+se, lty = 4)
 lines(a, pwr-se, lty = 4)


## ----t------------------------------------------------------------------------

# t(v)
pwr_t = function(v){
 
 alpha = 0.1
 n = 20
 m = 1e3
 N = length(v)
 pwr = numeric(N)
 cv = qnorm(1-alpha/2, 0, sqrt(6*(n-2) / ((n+1)*(n+3))))
 
 for (j in 1:N) { 
  sktests = numeric(m)
  for (i in 1:m) { 
   x = rt(n,v[j])
   sktests[i] = as.integer(abs(sk(x))>= cv)
  }
  pwr[j] = mean(sktests)
 }
 se = sqrt(pwr*(1-pwr) / m) 
  return(list(pwr = pwr,se = se))
}

v = seq(1,20)
pwr = pwr_t(v)$pwr
se = pwr_t(v)$se
# plot the power
plot(v, pwr, type = "b", xlab = "v", ylab = "pwr", ylim = c(0,1),pch=16)
abline(h = 0.1, lty = 2)
lines(v, pwr+se, lty = 4)
lines(v, pwr-se, lty = 4)


## -----------------------------------------------------------------------------
sigma1 <- 1
sigma2 <- 1.5
m <- 10000
n <- c(10, 20, 30, 50, 100, 500)

count5test <- function(x, y) {
X <- x - mean(x)
Y <- y - mean(y)
outx <- sum(X > max(Y)) + sum(X < min(Y))
outy <- sum(Y > max(X)) + sum(Y < min(X))
# return 1 (reject) or 0 (do not reject H0)
return(as.integer(max(c(outx, outy)) > 5))
}

power1 <- numeric(length(n))
for (i in 1:length(n)) {
  power1[i] <- mean(replicate(m, expr={
    x <- rnorm(n[i], 0, sigma1)
    y <- rnorm(n[i], 0, sigma2)
    count5test(x, y)
}))
}

sigma1 <- 1
sigma2 <- 1.5
m <- 10000
n <- c(10, 20, 30, 50, 100, 500)
power2 <- numeric(length(n))
for (i in 1:length(n)) {
  pvalues <- replicate(m, expr = {
    x <- rnorm(n[i], 0, sigma1)
    y <- rnorm(n[i], 0, sigma2)
    ftest <- var.test(x,y)
    ftest$p.value } )
  power2[i] <- mean(pvalues <= .055)
}
data.frame(n=n, C5=power1, Fp=power2)

## -----------------------------------------------------------------------------
library(MASS)
Mardia<-function(mydata){
  n = nrow(mydata)
  c = ncol(mydata)
  central <- mydata
  for(i in 1:c){
    central[,i] <- mydata[,i]-mean(mydata[,i])
  }
  sigmah <- t(central)%*%central/n
  a <- central%*%solve(sigmah)%*%t(central)
  b <- sum(colSums(a^{3}))/(n*n)
  test <- n*b/6
  chi <- qchisq(0.95,c*(c+1)*(c+2)/6)
  as.integer(test>chi)
}

set.seed(12345)
mu <- c(0,0,0)
sigma <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)
m = 1000
n <- c(10, 20, 30, 50, 100, 500)
#m: number of replicates; n: sample size
a=numeric(length(n))
for(i in 1:length(n)){
  a[i]=mean(replicate(m, expr={
    mydata <- mvrnorm(n[i],mu,sigma) 
    Mardia(mydata)
  }))
}

print(a) #calculate the t1e


## -----------------------------------------------------------------------------
library(MASS)
set.seed(12345)
mu1 <- mu2 <- c(0,0,0)
sigma1 <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)
sigma2 <- matrix(c(100,0,0,0,100,0,0,0,100),nrow=3,ncol=3)
sigma=list(sigma1,sigma2)
m=1000
n=50
#m: number of replicates; n: sample size
epsilon <- c(seq(0, .06, .01), seq(.1, 1, .05))
N <- length(epsilon)
pwr <- numeric(N)
for (j in 1:N) { #for each epsilon
  e <- epsilon[j]
  sktests <- numeric(m)
  for (i in 1:m) { #for each replicate
    index=sample(c(1, 2), replace = TRUE, size = n, prob = c(1-e, e))
    mydata<-matrix(0,nrow=n,ncol=3)
    for(t in 1:n){
      if(index[t]==1) mydata[t,]=mvrnorm(1,mu1,sigma1) 
      else mydata[t,]=mvrnorm(1,mu2,sigma2)
    }
    sktests[i] <- Mardia(mydata)
  }
  pwr[j] <- mean(sktests)
}
plot(epsilon, pwr, type = "b",
     xlab = bquote(epsilon), ylim = c(0,1))
abline(h = .05, lty = 3)
se <- sqrt(pwr * (1-pwr) / m) #add standard errors
lines(epsilon, pwr+se, lty = 3)
lines(epsilon, pwr-se, lty = 3)

## -----------------------------------------------------------------------------
library(bootstrap)
data(law, package = "bootstrap")
n <- nrow(law)
LSAT <- law$LSAT
GPA <- law$GPA
cor.hat <- cor(LSAT,GPA)
#compute the jackknife replicates, leave-one-out estimates
cor.jack <- numeric(n)
for (i in 1:n)
  cor.jack[i] <- cor(LSAT[-i],GPA[-i])
bias <- (n - 1) * (mean(cor.jack) - cor.hat)

se <- sqrt((n-1) *mean((cor.jack - mean(cor.jack))^2))
cat("bias_jack:",bias,"\n","se_jack:",se)

## -----------------------------------------------------------------------------
library(boot)
data(aircondit, package = "boot")
alpha <- 0.05
theta.boot <- function(hours, ind) {
#function to compute the statistic
x <- hours[ind, ]
mean(x)
}
boot.obj <- boot(aircondit, statistic = theta.boot, R = 2000)
print(boot.obj)
print(boot.ci(boot.obj,
type = c("basic", "norm", "perc","bca")))


## -----------------------------------------------------------------------------
library(bootstrap)
data(scor, package = "bootstrap")
lambda_hat <- eigen(cov(scor))$values
theta_hat <- lambda_hat[1] / sum(lambda_hat)
n <- nrow(scor) # number of rows (data size)
theta_j <- rep(0, n)
for (i in 1:n) {
  x <- scor [-i,]
  lambda <- eigen(cov(x))$values
  theta_j[i] <- lambda[1] / sum(lambda)
  # the i-th entry of theta_j is the i-th "leave-one-out" estimation of theta
}
bias_jack <- (n - 1) * (mean(theta_j) - theta_hat)
# the estimated bias of theta_hat, using jackknife
se_jack <- (n - 1) * sqrt(var(theta_j) / n)
# the estimated se of theta_hat, using jackknife
cat("theta.hat: ",theta_hat,"\n","bias_jack:",bias_jack,"\n","se_jack:",se_jack)

## -----------------------------------------------------------------------------
library(DAAG); attach(ironslag)
n <- length(magnetic) #in DAAG ironslag
e1 <- e2 <- e3 <- e4 <- numeric(n-1)
# for n-fold cross validation
# fit models on leave-one-out samples
for (k in 1:n-1) {
y <- magnetic[-k:-(k+1)]
x <- chemical[-k:-(k+1)]
J1 <- lm(y ~ x)
yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
yhat2 <- J1$coef[1] + J1$coef[2] * chemical[k+1]
e1[k] <- magnetic[k] - yhat1
e1[k+1] <- magnetic[k+1] - yhat2
J2 <- lm(y ~ x + I(x^2))
yhat3 <- J2$coef[1] + J2$coef[2] * chemical[k] +
J2$coef[3] * chemical[k]^2
yhat4 <- J2$coef[1] + J2$coef[2] * chemical[k+1] +
J2$coef[3] * chemical[k+1]^2
e2[k] <- magnetic[k] - yhat3
e2[k+1] <- magnetic[k+1] - yhat4
J3 <- lm(log(y) ~ x)
logyhat5 <- J3$coef[1] + J3$coef[2] * chemical[k]
logyhat6 <- J3$coef[1] + J3$coef[2] * chemical[k+1]
yhat5 <- exp(logyhat5)
yhat6 <- exp(logyhat6)
e3[k] <- magnetic[k] - yhat5
e3[k+1] <- magnetic[k+1] - yhat6
J4 <- lm(log(y) ~ log(x))
logyhat7 <- J4$coef[1] + J4$coef[2] * log(chemical[k])
logyhat8 <- J4$coef[1] + J4$coef[2] * log(chemical[k+1])
yhat7 <- exp(logyhat7)
yhat8 <- exp(logyhat8)
e4[k] <- magnetic[k] - yhat7
e4[k+1] <- magnetic[k+1] - yhat8
}
c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))
J2

## -----------------------------------------------------------------------------

set.seed(12345)

# Count Five test
count5test = function(x, y) {
X = x - mean(x)
Y = y - mean(y)
outx = sum(X > max(Y)) + sum(X < min(Y))
outy = sum(Y > max(X)) + sum(Y < min(X))
# return 1 (reject) or 0 (do not reject H0)
return(as.integer(max(c(outx, outy)) > 5))
}
# Count Five test permutation
count5test_permutation = function(z) {

n = length(z)
x = z[1:(n/2)]
y = z[-(1:(n/2))]
X = x - mean(x)
Y = y - mean(y)
outx = sum(X > max(Y)) + sum(X < min(Y)) 
outy = sum(Y > max(X)) + sum(Y < min(X))
# return 1 (reject) or 0 (do not reject H0) 
return(as.integer(max(c(outx, outy)) > 5))
}
permutation = function(z,R) {
  n = length(z)
  out = numeric(R)
  for (r in 1: R){
      p = sample(1:n ,n ,replace = FALSE)
      out[r] = count5test_permutation(z[p])
  }
  sum(out)/R
}              


n1 = 20
n2 = 50
mu1 = mu2 = 0
sigma1 = sigma2 = 1
m = 1e3

alphahat1 = mean(replicate(m, expr={
x = rnorm(n1, mu1, sigma1)
y = rnorm(n2, mu2, sigma2)
x = x - mean(x) #centered by sample mean
y = y - mean(y)
count5test(x, y)
}))
alphahat2 = mean(replicate(m, expr={
x = rnorm(n1, mu1, sigma1)
y = rnorm(n2, mu2, sigma2)
x = x - mean(x) #centered by sample mean 
y = y - mean(y)
z = c(x,y)
permutation(z,1000) 
})<0.05)

round(c(count5test=alphahat1,count5test_permutation=alphahat2),4)


## -----------------------------------------------------------------------------
library(RANN)
library(boot)
library(Ball)
library(energy)
library(MASS)

Tn <- function(z, ix, sizes,k) {
  n1 <- sizes[1]
  n2 <- sizes[2]
  n <- n1 + n2
  if(is.vector(z)) z <- data.frame(z,0)
  z <- z[ix, ]
  NN <- nn2(data=z, k=k+1) # what's the first column?
  block1 <- NN$nn.idx[1:n1,-1]
  block2 <- NN$nn.idx[(n1+1):n,-1]
  i1 <- sum(block1 < n1 + .5) 
  i2 <- sum(block2 > n1+.5)
  return((i1 + i2) / (k * n))
}

eqdist.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z,statistic=Tn,R=R,
                   sim = "permutation", sizes = sizes,k =k)
  ts <- c(boot.obj$t0,boot.obj$t)
  p.value <- mean(ts>=ts[1])
  list(statistic=ts[1],p.value=p.value)
  }
#Unequal variances and equal expectations:
mu1 <- c(0,0,0)
sigma1 <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)
mu2 <- c(0,0,0)
sigma2 <- matrix(c(2,0,0,0,3,0,0,0,4),nrow=3,ncol=3)
n1=n2=20
n <- n1+n2 
N = c(n1,n2)
k=3
R=999
m=100
set.seed(12345)
p.values <- matrix(NA,m,3)
for(i in 1:m){
  mydata1 <- mvrnorm(n1,mu1,sigma1)
  mydata2 <- mvrnorm(n2,mu2,sigma2)
  mydata <- rbind(mydata1,mydata2)
  p.values[i,1] <- eqdist.nn(mydata,N,k)$p.value
  p.values[i,2] <- eqdist.etest(mydata,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=mydata1,y=mydata2,num.permutations=R,seed=i*2846)$p.value
}
alpha <- 0.05;
pow1 <- colMeans(p.values<alpha)
pow1


## -----------------------------------------------------------------------------
#Non-normal distributions: t distribution with 1 df
m <- 100; k<-3; set.seed(12345)
n1 <- n2 <- 20; R<-999; n <- n1+n2; N = c(n1,n2)
p.values <- matrix(NA,m,3)
x <- numeric(n1)
y <- numeric(n2)
for(i in 1:m){
  mydata1 <- as.matrix(rt(n1,1,2),ncol=1)
  mydata2 <- as.matrix(rt(n2,2,5),ncol=1)
  mydata <- rbind(mydata1,mydata2)
  p.values[i,1] <- eqdist.nn(mydata,N,k)$p.value
  p.values[i,2] <- eqdist.etest(mydata,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=mydata1,y=mydata2,num.permutations=R,seed=i*12345)$p.value
}
alpha <- 0.05;
pow2 <- colMeans(p.values<alpha)
pow2

## -----------------------------------------------------------------------------
#Non-normal distributions: bimodel distribution
m <- 100; k<-3; p<-2; set.seed(12345)
n1 <- n2 <- 20; R<-999; n <- n1+n2; N = c(n1,n2)
p.values <- matrix(NA,m,3)
rbimodel<-function(n,mu1,mu2,sd1,sd2){
  index=sample(1:2,n,replace=TRUE)
  x=numeric(n)
  index1<-which(index==1)
  x[index1]<-rnorm(length(index1), mu1, sd1)
  index2<-which(index==2)
  x[index2]<-rnorm(length(index2), mu2, sd2)
  return(x)
}
for(i in 1:m){
  mydata1 <- as.matrix(rbimodel(n1,0,0,1,2),ncol=1)
  mydata2 <- as.matrix(rbimodel(n2,1,1,4,3),ncol=1)
  mydata <- rbind(mydata1,mydata2)
  p.values[i,1] <- eqdist.nn(mydata,N,k)$p.value
  p.values[i,2] <- eqdist.etest(mydata,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=mydata1,y=mydata2,num.permutations=R,seed=i*2846)$p.value
}
alpha <- 0.05;
pow3 <- colMeans(p.values<alpha)
pow3

## -----------------------------------------------------------------------------
#Unbalanced samples:
m <- 100; k<-3;set.seed(12345)
n1 <- 10; n2 <- 100; R<-999; n <- n1+n2; N = c(n1,n2)
mu1 <- c(0,0,0)
sigma1 <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)
mu2 <- c(0.5,-0.5,0.5)
sigma2 <- matrix(c(2,0,0,0,2,0,0,0,2),nrow=3,ncol=3)
p.values <- matrix(NA,m,3)
for(i in 1:m){
  mydata1 <- mvrnorm(n1,mu1,sigma1)
  mydata2 <- mvrnorm(n2,mu2,sigma2)
  mydata <- rbind(mydata1,mydata2)
  p.values[i,1] <- eqdist.nn(mydata,N,k)$p.value
  p.values[i,2] <- eqdist.etest(mydata,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=mydata1,y=mydata2,num.permutations=R,seed=i*2846)$p.value
}
alpha <- 0.05;
pow4 <- colMeans(p.values<alpha)
pow4

## -----------------------------------------------------------------------------
rw.Metropolis <- function(sigma, x0, N) {
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i] <= exp(abs(x[i-1])- abs(y)))
      x[i] <- y else {
        x[i] <- x[i-1]
        k <- k + 1
      }
    }
  return(list(x=x, k=k))
  }

N <- 2000
sigma <- c(.05, .5, 2, 16)
x0 <- 25
rw1 <- rw.Metropolis(sigma[1], x0, N)
rw2 <- rw.Metropolis(sigma[2], x0, N)
rw3 <- rw.Metropolis(sigma[3], x0, N)
rw4 <- rw.Metropolis(sigma[4], x0, N)

print(c(rw1$k, rw2$k, rw3$k, rw4$k))
print(c(rw1$k/2000, rw2$k/2000, rw3$k/2000, rw4$k/2000))
print(c(1-rw1$k/2000, 1-rw2$k/2000, 1-rw3$k/2000, 1-rw4$k/2000))

plot(rw1$x,type = "l")
plot(rw2$x,type = "l")
plot(rw3$x,type = "l")
plot(rw4$x,type = "l")

## -----------------------------------------------------------------------------
Gelman.Rubin <- function(psi) {
# psi[i,j] is the statistic psi(X[i,1:j])
# for chain in i-th row of X
psi <- as.matrix(psi)
n <- ncol(psi)
k <- nrow(psi)
psi.means <- rowMeans(psi) #row means
B <- n * var(psi.means) #between variance est.
psi.w <- apply(psi, 1, "var") #within variances
W <- mean(psi.w) #within est.
v.hat <- W*(n-1)/n + (B/n) #upper variance est.
r.hat <- v.hat / W #G-R statistic
return(r.hat)
}

par(mar=c(1,1,1,1))
k <- 4    # four chains
x0 <- c(-10,-5,5,10)    # overdispersed initial values
N <- 10000    # length of chains
b <- 200    # burn-in length

par(mfrow=c(2,2))

X <- matrix(nrow=k,ncol=N)
for (i in 1:k)
  X[i,] <- rw.Metropolis(0.5,x0[i],N)$x
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
psi[i,] <- psi[i,] / (1:ncol(psi))
rhat <- rep(0, N)
for (j in (1000+1):N)
rhat[j] <- Gelman.Rubin(psi[,1:j])
plot(rhat[(1000+1):N], type="l", xlab="sigma=0.5", ylab="R_hat")
abline(h=1.2, lty=2)

X <- matrix(nrow=k,ncol=N)
for (i in 1:k)
  X[i,] <- rw.Metropolis(1,x0[i],N)$x
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
psi[i,] <- psi[i,] / (1:ncol(psi))
rhat <- rep(0, N)
for (j in (500+1):N)
rhat[j] <- Gelman.Rubin(psi[,1:j])
x2 <- min(which(rhat>0 & rhat<1.2))
plot(rhat[(500+1):N], type="l", xlab="sigma=1", ylab="R_hat")
abline(h=1.2, lty=2)

X <- matrix(nrow=k,ncol=N)
for (i in 1:k)
  X[i,] <- rw.Metropolis(4,x0[i],N)$x
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
psi[i,] <- psi[i,] / (1:ncol(psi))
rhat <- rep(0, N)
for (j in (b+1):N)
rhat[j] <- Gelman.Rubin(psi[,1:j])
x3 <- min(which(rhat>0 & rhat<1.2))
plot(rhat[(b+1):N], type="l", xlab="sigma=4", ylab="R_hat")
abline(h=1.2, lty=2)

X <- matrix(nrow=k,ncol=N)
for (i in 1:k)
  X[i,] <- rw.Metropolis(16,x0[i],N)$x
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
psi[i,] <- psi[i,] / (1:ncol(psi))
rhat <- rep(0, N)
for (j in (b+1):N)
rhat[j] <- Gelman.Rubin(psi[,1:j])
x4 <- min(which(rhat>0 & rhat<1.2))
plot(rhat[(b+1):N], type="l", xlab="sigma=16", ylab="R_hat")
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
k <- c(4:25, 100,500, 1000)
out_root <- numeric(length(k))
out_y <- numeric(length(k))
for (i in 1:length(k)){
  f <- function(a){
    pt(sqrt(a^2*(k[i]-1)/(k[i]-a^2)),df =k[i]-1)-pt(sqrt(a^2*k[i]/(k[i]+1-a^2)),df = k[i])}
  out_root[i] <- uniroot(f,c(1e-10,sqrt(k[i])-1e-10))$root
  out_y[i] <- 1- pt(sqrt(out_root[i]^2*(k[i]-1)/(k[i]-out_root[i]^2)),df =k[i]-1)
}
print(cbind(out_root,out_y))


## -----------------------------------------------------------------------------
plot(1:length(k),type = "l", out_root, xlab = " ", ylab = expression(A(k)),main = "points of insection")

## -----------------------------------------------------------------------------

library(nloptr)
# Mle 
eval_f0 = function(x,x1,n.A=444,n.B=132,nOO=361,nAB=63) {
  
  r1 = 1-sum(x1)
  nAA = n.A*x1[1]^2/(x1[1]^2+2*x1[1]*r1)
  nBB = n.B*x1[2]^2/(x1[2]^2+2*x1[2]*r1)
  r = 1-sum(x)
  return(-2*nAA*log(x[1])-2*nBB*log(x[2])-2*nOO*log(r)-
           (n.A-nAA)*log(2*x[1]*r)-(n.B-nBB)*log(2*x[2]*r)-nAB*log(2*x[1]*x[2]))
}


# constraint
eval_g0 = function(x,x1,n.A=444,n.B=132,nOO=361,nAB=63) {
  return(sum(x)-0.999999)
}

opts = list("algorithm"="NLOPT_LN_COBYLA",
             "xtol_rel"=1.0e-8)
mle = NULL
r = matrix(0,1,2)
r = rbind(r,c(0.2,0.35))# the beginning value of p0 and q0
j = 2
while (sum(abs(r[j,]-r[j-1,]))>1e-8) {
res = nloptr( x0=c(0.2,0.25),
               eval_f=eval_f0,
               lb = c(0,0), ub = c(1,1), 
               eval_g_ineq = eval_g0, 
               opts = opts, x1=r[j,],n.A=444,n.B=132,nOO=361,nAB=63 )
j = j+1
r = rbind(r,res$solution)
mle = c(mle,eval_f0(x=r[j,],x1=r[j-1,]))
}
#the result of EM algorithm
r 
#the max likelihood values
plot(-mle,type = 'l')

## -----------------------------------------------------------------------------
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

# For loop
modle_loop <- vector("list", length = length(formulas))
i <- 1
for (formula in formulas) { 
  modle_loop[[i]] <- lm(formula, data = mtcars); 
  i <- i + 1 
  }
modle_loop

# lapply
modle_lapply <- lapply(formulas, lm, data = mtcars) 
modle_lapply

## -----------------------------------------------------------------------------
trials <- replicate(
  100, 
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)
sapply(trials, function(x) x$p.value)


### Extra challenge: get rid of the anonymous function by using [[ directly.

sapply(trials, `[[`, 'p.value')

## -----------------------------------------------------------------------------
lapply_variant <- function(x, f, ...) {
  indices <- sample(seq_along(x))
  raw1 <- vapply(indices, function(i) f(x[[i]], ...), numeric(1))
  out <- vector("list", length(x))
  for (j in 1:5) {
    out[indices[j]] <- raw1[j] 
  }
  out
}

all.equal(lapply(1:5, function(x) x^2), lapply_variant(1:5, function(x) x^2))

## -----------------------------------------------------------------------------
library(Rcpp) # Attach R package "Rcpp"
    # Define function "add"
cppFunction('// Add code below into C++ file Rcpp_example.cpp
#include <Rcpp.h>
using namespace Rcpp;

// Place the export tag right above function declaration.
//[[Rcpp::export]]
NumericVector Metropolis( double sigma, double x0, int N) {
  NumericVector x(N);
  NumericVector u = runif(N);
  x[0] = x0;
  for (int i=1; i<N; i++) { 
    NumericVector y = rnorm(1, x[i-1], sigma);
    if (u[i] <= exp(abs(x[i-1])- abs(y[0]))){
        x[i] = y[0]; 
    }
    else {
        x[i] = x[i-1];
    }
  }
  return(x);
}
')
library(microbenchmark)
rw.Metropolis <- function(sigma, x0, N) {
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i] <= exp(abs(x[i-1])- abs(y)))
      x[i] <- y else {
        x[i] <- x[i-1]
        k <- k + 1
      }
    }
  return(list(x=x, k=k))
}

ts <- microbenchmark(meanR=rw.Metropolis(2, 25, 1000),meancpp=Metropolis(2, 25, 1000))
summary(ts)[,c(1,3,5,6)]

set.seed(12345)
x0 = 25
N = 2000
sigma = 2
rwR = rw.Metropolis(sigma,x0,N)$x[-(1:500)]
rwC = Metropolis(sigma,x0,N)[-(1:500)]
qqplot(rwR,rwC)
abline(a=0,b=1,col='black')

