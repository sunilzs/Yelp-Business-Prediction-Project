library(datasets)
library(RWeka)
library(caret)
library(class) 
library(e1071)
library(partykit)
library(leaderCluster)
library(ggplot2)
library(ggmap)
library(fields)

setwd("C:/Users/Machine/Desktop/Pattern")
yelp_train = read.csv("PAbusiness_training_cluster.csv")

summary(yelp_train)

trainData <- yelp_train[,c("business_id","latitude","longitude")]
train=trainData[,c(2:3)]
summary(train)



lat = trainData[,c(2)]
lon = trainData[,c(3)]


mapgilbert <- get_map(location = c(lon = mean(trainData$longitude), lat = mean(trainData$latitude)), zoom = 12,
                      maptype = "hybrid", scale = 2)

threshold.in.km <- 0.15
coors <- data.frame(lon,lat)

#distance matrix
dist <- rdist.earth(coors,miles = F,R=6371)

#clustering
fit <- hclust(as.dist(dist), method = "single")
clusters <- cutree(fit,h = threshold.in.km)



ggmap(mapgilbert) +
  geom_point(data = trainData, aes(x = lon, y = lat, fill=scale_colour_manual(values=c("8" = "red",
                                                                                       "4" = "blue","6" = "green")), alpha = 0.1), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

tab1clustn <-data.frame(lat,lon, clusters)
op <- data.frame(trainData[1], tab1clustn)

write.csv(op, file = "PA_business_clusters.csv")

# tab=(table(lat,lon, clusters))
# out <- cbind(lat, lon, clusterNum = clusters$cluster)

plot(lat, lon, col = clusters, pch = 20)
