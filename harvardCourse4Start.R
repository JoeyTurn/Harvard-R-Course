library(devtools)
install_github("genomicsclass/tissuesGeneExpression")
library(tissuesGeneExpression)
data(tissuesGeneExpression)
head(e)
head(tissue)
table(tissue)
d <- dist(t(e))
image(as.matrix(d))

as.matrix(d)[3, 45]
x <- e[,3]
y <- e[, 45]
sqrt(crossprod(x-y))

x<-e["210486_at",]
y<-e["200805_at",]
sqrt(crossprod(x-y))

dim(e)
dim(as.matrix(d))
as.dist(t(e))
length(d)

as.matrix(d)[3, 3]
