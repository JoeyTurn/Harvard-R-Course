A <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0,
              1, 1, 1, 0,
              1, 1, 0, 1), 4, 4, byrow = TRUE)
B <- matrix(c(1, 0, 0, 1,
              1, 0, 1, 1,
              1, 1, 0, 0,
              1, 1, 1, 0), 4, 4, byrow = TRUE)
C <- matrix(c(1, 0, 0, 
              1, 1, 2, 
              1, 2, 4, 
              1, 3, 6), 4, 3, byrow = TRUE)
D <- matrix(c(1, 0, 0, 0, 0,
              1, 0, 0, 0, 1,
              1, 1, 0, 1, 0,
              1, 1, 0, 1, 1,
              1, 0, 1, 1, 0,
              1, 0, 1, 1, 1), 6, 5, byrow = TRUE)
E <- matrix(c(1, 0, 0, 0,
              1, 0, 1, 0,
              1, 1, 0, 0,
              1, 1, 1, 1), 4, 4, byrow = TRUE)
F <- matrix(c(1, 0, 0, 1,
              1, 0, 0, 1,
              1, 0, 1, 1,
              1, 1, 0, 0,
              1, 1, 0, 0,
              1, 1, 1, 0), ncol = 4, byrow = TRUE)
sex <- factor(rep(c("female","male"),each=4))
trt <- factor(c("A","A","B","B","C","C","D","D"))
X <- model.matrix( ~ sex + trt)
qr(X)$rank
Y <- 1:8
makeYstar <- function(a,b) Y - X[,2] * a - X[,5] * b
fitTheRest <- function(a,b) {
  Ystar <- makeYstar(a,b)
  Xrest <- X[,-c(2,5)]
  betarest <- solve(t(Xrest) %*% Xrest) %*% t(Xrest) %*% Ystar
  residuals <- Ystar - Xrest %*% betarest
  sum(residuals^2)
}
fitTheRest(1, 2)
expand.grid(1:3,1:3)
betas = expand.grid(-2:8,-2:8)
rss = apply(betas,1,function(x) fitTheRest(x[1],x[2]))
library(rafalib)
## plot the pairs what are minimum
themin=min(rss)
plot(betas[which(rss==themin),])



fit <- lm(friction ~ type + leg, data=spider)
betahat <- coef(fit)
Y <- matrix(spider$friction, ncol=1)

X <- model.matrix(~ type + leg, data=spider)
R <- qr.R(qr(X))
Q <- qr.Q(qr(X))
(betahat <- backsolve(R, crossprod(Q,Y) ) )
