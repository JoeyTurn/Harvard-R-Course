library(UsingR)
x <- father.son$fheight
y <- father.son$sheight
n <- length(y)
N <- 50
RNGkind(sample.kind = "Rounding")
set.seed(1)
index <- sample(n,N)
sampledat <- father.son[index,]
x <- sampledat$fheight
y <- sampledat$sheight
betahat <- lm(y~x)$coef[2]
seb <- sqrt(cov(sampledat))

fit <- lm(y ~ x)
yresid <- fit$fitted.values #residuals
sigma2 <- sum((y-yresid)^2)/48 #cov var of y, divisor is N-p (p=number of terms, in this case intercept & slope)
X <- cbind(rep(1,N), x)
Xdiags <- diag(solve(t(X) %*% X))

sqrt(sigma2 * Xdiags) #the SE terms of intercept, and slope

X <- cbind(rep(1, 5 + 7))
X = cbind(rep(1, 12),
          rep(c(0, 1),
              c(5, 7)))
t(X) %*% X
