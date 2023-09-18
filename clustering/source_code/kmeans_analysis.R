# author: Hamza Reza Pavel
# ID: 1001741797
# Group: 2
# Seed Value: 06271993
# Data subset: year-> 2006, 2008, 2009. Months: February, August

#imports go here
library(dplyr)
library(VIM)
library(magrittr)
library(clusteval)
library(factoextra)


set.seed(06271993)


y2006_Feb <- read.csv("y2006_Feb.csv", sep = "," , header = T, stringsAsFactors= T)
y2006_Aug <- read.csv("y2006_Aug.csv", sep = "," , header = T, stringsAsFactors= T)

y2008_Feb <- read.csv("y2008_Feb.csv", sep = "," , header = T, stringsAsFactors= T)
y2008_Aug <- read.csv("y2008_Aug.csv", sep = "," , header = T, stringsAsFactors= T)

y2009_Feb <- read.csv("y2009_Feb.csv", sep = "," , header = T, stringsAsFactors= T)
y2009_Aug <- read.csv("y2009_Aug.csv", sep = "," , header = T, stringsAsFactors= T)


y2006_Feb$STN <- as.factor(y2006_Feb$STN)
y2006_Aug$STN <- as.factor(y2006_Aug$STN)

y2008_Feb$STN <- as.factor(y2008_Feb$STN)
y2008_Aug$STN <- as.factor(y2008_Aug$STN)

y2009_Feb$STN <- as.factor(y2009_Feb$STN)
y2009_Aug$STN <- as.factor(y2009_Aug$STN)


#scale the dataframes
y2006_Feb[, -c(1)] <- scale(y2006_Feb[, -c(1)])
y2006_Aug[, -c(1)] <- scale(y2006_Aug[, -c(1)])

y2008_Feb[, -c(1)] <- scale(y2008_Feb[, -c(1)])
y2008_Aug[, -c(1)] <- scale(y2008_Aug[, -c(1)])

y2009_Feb[, -c(1)] <- scale(y2009_Feb[, -c(1)])
y2009_Aug[, -c(1)] <- scale(y2009_Aug[, -c(1)])


fviz_nbclust(y2006_Feb[, -c(1)], kmeans, method = "wss")
 

measure_cluster_similarity<-function(x, y){
  shortest <- min(length(x), length(y))
  y <- head(y, shortest)
  x <- head(x, shortest)
  res = cluster_similarity(x, y, similarity = c("jaccard"))
  return(res)
}

km.res1 <- kmeans(y2006_Feb[, -c(1)], 7, nstart = 50)
km.res2 <- kmeans(y2006_Feb[, -c(1)], 6, nstart = 50)


res = measure_cluster_similarity(km.res1$cluster, km.res2$cluster)

