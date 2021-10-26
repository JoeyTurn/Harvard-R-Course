install.packages("UsingR")
library(UsingR)
library(dplyr)
data("father.son",package="UsingR")
mean(father.son$sheight)
fso <- father.son %>%
  filter(round(fheight) == 71)

mean(fso$sheight)
X = matrix(1:1000,100,10)
X[25, 3]
ott <- seq(1,10)
Y <- cbind(X1=ott, X2=ott*2, X3=ott*3, X4=ott*4, X5=ott*5)
sum(Y[7,])

x <- matrix(c(3, 4, -5, 1, 2, 2, 2, -1, 1, -1, 5, -5, 5, 0, 0, 1), 4, 4, byrow = TRUE)
y <- matrix(c(10, 5, 7, 4), 4, 1)
xy <- solve(x) %*% y

a <- matrix(1:12, nrow=4)
b <- matrix(1:15, nrow=3)
ab <- a%*%b
ab[3, 2]

X <- matrix(c(1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,1,1),nrow=6)
rownames(X) <- c("a","a","b","b","c","c")
beta <- c(10,3,-3)
fitteda <- X %*% beta


g = 9.8 ## meters per second
h0 = 56.67
v0 = 0
n = 25
tt = seq(0,3.4,len=n) ##time in secs, t is a base function
y = h0 + v0 *tt  - 0.5* g*tt^2 + rnorm(n,sd=1)

X = cbind(1,tt,tt^2)
A = solve(crossprod(X))%*%t(X)
A %*% y

gizza <- function(n, h0 = 56.67, v0 = 0) {
  tt = seq(0,3.4,len=n)
  y = h0 + v0 *tt  - 0.5* g*tt^2 + rnorm(n,sd=1)
  X = cbind(1,tt,tt^2)
  A = solve(crossprod(X))%*%t(X) %*% y
  return (-2*A[3])
}

sim <- replicate(10000, gizza(25))
sd(sim)

x <- father.son$fheight
y <- father.son$sheight
n <- length(y)

index <- sample(n, N)
sampledat <- father.son[index, ]
x <- father.son$fheight
y <- father.son$sheight
bhat <- lm(y~x)$coef

N =  50

RNGkind(sample.kind = "Rounding")
set.seed(1)

getbhat <- function(N, n= 1078) {
  index <- sample(n, N)
  sampledat <- father.son[index, ]
  x <- sampledat$fheight
  y <- sampledat$sheight
  bhat <- lm(y~x)$coef[2]
  return (bhat)
}

mcsim <- replicate(10000, getbhat(50))

sd(mcsim)

X <- father.son$fheight
Y <- father.son$sheight

mean((Y - mean(Y))*(X-mean(X)))
