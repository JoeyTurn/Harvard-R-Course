url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv(filename, skip=1)

spider$log2friction <- log2(spider$friction)
boxplot(log2friction ~ type*leg, data=spider)

spiderlm <- lm(log2friction ~ leg*type, data = spider)
X <- model.matrix(~ type*leg, data=spider)
summary(spiderlm)
anova(spiderlm)
contrast(spiderlm, list(type="push", leg='L2'), list(type="push", leg='L1'))

RNGkind(sample.kind = "Rounding")
set.seed(1)

N <- 40
p <- 4
group <- factor(rep(1:p,each=N/p))
X <- model.matrix(~ group)
fstat = function(N, p=4) {
  Y <- rnorm(N,mean=42,7)
  mu0 <- mean(Y)
  initial.ss <- sum((Y - mu0)^2)
  s <- split(Y, group)
  after.group.ss <- sum(sapply(s, function(x) sum((x - mean(x))^2)))
  (group.ss <- initial.ss - after.group.ss)
  group.ms <- group.ss / (p - 1)
  after.group.ms <- after.group.ss / (N - p)
  f.value <- group.ms / after.group.ms
}
Fs <- replicate(1000, fstat(40))
hist(Fs, col="grey", border="white", breaks=50, freq=FALSE)
xs <- seq(from=0,to=6,length=100)
lines(xs, df(xs, df1 = p - 1, df2 = N - p), col="red")
