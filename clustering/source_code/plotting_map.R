library(dplyr)
library(VIM)
library(magrittr)
library(clusteval)
library(factoextra)
library(ggmap)


set.seed(06271993)
#set.seed(39917260)


y2006_Feb <- read.csv("y2006_Feb.csv", sep = "," , header = T, stringsAsFactors= T)
y2006_Aug <- read.csv("y2006_Aug.csv", sep = "," , header = T, stringsAsFactors= T)

y2008_Feb <- read.csv("y2008_Feb.csv", sep = "," , header = T, stringsAsFactors= T)
y2008_Aug <- read.csv("y2008_Aug.csv", sep = "," , header = T, stringsAsFactors= T)

y2009_Feb <- read.csv("y2009_Feb.csv", sep = "," , header = T, stringsAsFactors= T)
y2009_Aug <- read.csv("y2009_Aug.csv", sep = "," , header = T, stringsAsFactors= T)

stationdf<-read.csv("stations-lat-long.csv", sep = "," , header = T, stringsAsFactors= T)
stationdf<-subset(stationdf, select=-c(Address, State, City, County, Zip))


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


#fviz_nbclust(y2006_Feb[, -c(1)], kmeans, method = "wss")
#fviz_nbclust(y2008_Feb[, -c(1)], kmeans, method = "wss")
#fviz_nbclust(y2009_Feb[, -c(1)], kmeans, method = "wss")

#km.res1 <- kmeans(y2006_Aug[, -c(1)], 6, nstart = 50)
#km.res2 <- kmeans(y2008_Aug[, -c(1)], 6, nstart = 50)
#km.res3 <- kmeans(y2009_Feb[, -c(1)], 7, nstart = 50)




measure_cluster_similarity<-function(kmres1, kmres2){
  x = kmres1$cluster
  y = kmres2$cluster
  shortest <- min(length(x), length(y))
  y <- head(y, shortest)
  x <- head(x, shortest)
  res = cluster_similarity(x, y, similarity = c("jaccard"))
  return(res)
}


y06feb <- kmeans(y2006_Feb[, -c(1)], 6, nstart = 50)
y06aug<- kmeans(y2006_Aug[, -c(1)], 6, nstart = 50)

y08feb <- kmeans(y2008_Feb[, -c(1)], 6, nstart = 50)
y08aug <- kmeans(y2008_Aug[, -c(1)], 6, nstart = 50)

y09feb <- kmeans(y2009_Feb[, -c(1)], 7, nstart = 50)
y09aug <- kmeans(y2009_Aug[, -c(1)], 7, nstart = 50)

#res = measure_cluster_similarity(y09feb, y09aug)
#print(res)

#process data for plots
y2006_Feb$cluster<-as.factor(y06feb$cluster)
y2006_Feb<-merge(y2006_Feb, stationdf, by="STN")

y2008_Feb$cluster<-as.factor(y08feb$cluster)
y2008_Feb<-merge(y2008_Feb, stationdf, by="STN")

y2009_Feb$cluster<-as.factor(y09feb$cluster)
y2009_Feb<-merge(y2009_Feb, stationdf, by="STN")

y2006_Aug$cluster<-as.factor(y06aug$cluster)
y2006_Aug<-merge(y2006_Aug, stationdf, by="STN")

y2008_Aug$cluster<-as.factor(y08aug$cluster)
y2008_Aug<-merge(y2008_Aug, stationdf, by="STN")

y2009_Aug$cluster<-as.factor(y09aug$cluster)
y2009_Aug<-merge(y2009_Aug, stationdf, by="STN")




texasmap <- get_map("Texas", zoom = 6,  maptype = "roadmap")

ggmap(texasmap) + geom_point(aes(x = Lon, y = Lat, colour = as.factor(cluster)), data = y2009_Aug) +  ggtitle("Weather Stations Aug, 2009")








