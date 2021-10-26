library(dplyr)
data(ChickWeight)
head(ChickWeight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)
chick = reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time",
                direction="wide")
head(chick)
chick = na.omit(chick)
x <- chick %>%
  filter(Diet == 1) %>%
  select(weight.4) %>%
  unlist
y <- chick %>%
  filter(Diet == 4) %>%
  select(weight.4) %>%
  unlist

t.test(x,y)
wilcox.test(x, y)

x <- c(x, 200)
t.test(x, y)$p.value
wilcox.test(x, y)$p.value

library(rafalib)
mypar(1,3)

boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)
t.test(x, y)$statistic
t.test(x, y+10)$statistic
t.test(x, y+100)$statistic
wilcox.test(x, y)$statistic
wilcox.test(x, y+10)$statistic
wilcox.test(x, y+100)$statistic
wilcox.test(c(1, 2, 3), c(400, 500, 600))$p.value
