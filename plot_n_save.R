library(ggplot2)
library(gridExtra)

jpeg('rplot.jpg')

a = runif(10,1,5)
b = runif(10,3,10)
k = as.data.frame(cbind(a,b))

train_fit <- ggplot(k, aes(c(1:length(k$a)),a)) +
  geom_line(color='red') +
  geom_line(aes(c(1:length(k$b)),b), color="green")

test_fit <- ggplot(k, aes(c(1:length(k$a)),a)) +
  geom_line(color='red') +
  geom_line(aes(c(1:length(k$b)),b), color="green")

grid.arrange(train_fit,test_fit ,ncol=1, nrow =2)

dev.off()

# check
# http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization
