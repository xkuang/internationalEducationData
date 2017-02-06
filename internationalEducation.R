library(tidyr, dplyr)
library(corrplot)
library(ggplot2)
library(rpart)
library(ROCR)
library(cluster)
library(mclust)
library(pvclust)
library(fpc)
library(readr)
D1 <- read_csv("~/internationalEducationData/141516internationalStudent.csv")
D1 <- read.table("141516internationalStudent.csv", sep = ",", header = TRUE)
D2 <- dplyr::select(D1, 1:3,5)
names(D2) <- c("place","2014","2015","change")
D3 <- na.omit(D2)
D3 <- scale(D3)
mydata <-D3
# Ward Hierarchical Clustering with Bootstrapped p values
fit <- pvclust(mydata, method.hclust="ward.D",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)

D4 <- tidyr::gather(D3, "variable","value")
names(D2) <- c("variable","value")
g <- ggplot(D2,aes(x=value))
#Facets divide a plot into subplots based on the values of one or more discrete variables
#g+geom_histogram(binwidth = diff(range(D2$value))/2000)+facet_wrap(~variable, scales = "free")
#code reference:  https://groups.google.com/forum/#!topic/ggplot2/rhPWQEFMx6A

g + geom_histogram(data = D2[D2$variable == "av.assignment.score",], binwidth=0.01) +
  geom_histogram(data = D2[D2$variable == "forum.posts",], binwidth=1) +  
  geom_histogram(data = D2[D2$variable == "level.up2",], binwidth=1) +
  geom_histogram(data = D2[D2$variable == "messages",], binwidth=1) + 
  geom_histogram(data = D2[D2$variable == "post.test.score",], binwidth=0.01) +
  geom_histogram(data = D2[D2$variable == "pre.test.score",], binwidth=0.01) + 
  facet_wrap(~variable, scales = "free")