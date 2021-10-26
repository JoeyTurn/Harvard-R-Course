install.packages("devtools")
library(devtools)
library(dplyr)
library(readr)
install_github("genomicsclass/GSE5859Subset")
library(GSE5859Subset)
GSE5859Subset <- na.omit(geneExpression)
data(GSE5859Subset) ##this loads the three tables
which(sampleInfo$date == "2005-06-10")
which(geneAnnotation$SYMBOL == "ARPC1A")
geneExpression[323, 15]
apply(geneExpression, c(8793, 24), "median")

set.seed(1)
library(downloader)
url = "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename = "femaleControlsPopulation.csv"
if (!file.exists(filename)) download(url,destfile=filename)
population = read.csv(filename)
pvals <- replicate(1000,{
  control = sample(population[,1],12)
  treatment = sample(population[,1],12)
  t.test(treatment,control)$p.val
})
head(pvals)
hist(pvals)
mean(pvals < .05)
mean(pvals < .01)
