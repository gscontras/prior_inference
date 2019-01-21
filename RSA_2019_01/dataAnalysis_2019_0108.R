x2ModelData <- read.csv("x2ModelsOptParams_2019_0114.csv")
x2MDKL <- read.csv("x2ModelsKLDivs_2019_0114.csv")

names(x2ModelData) <- c("X", "V1", "P1","P2","P3","P23_2","P23_3","P13_1","P13_3","P12_1","P12_2","P123_1","P123_2","P123_3")
names(x2MDKL) <- c("X","V1","KL_Base","KL_1","KL_2","KL_3","KL_23","KL_13","KL_12","KL_123")

x2MDall <- merge(x2ModelData, x2MDKL, by.x = "V1", by.y = "V1")
x2MDall <- x2MDall[which(colnames(x2MDall)!="X.x")]
x2MDall <- x2MDall[which(colnames(x2MDall)!="X.y")]

write.csv(x2MDall, "x2ModelValuesAll_2019_0114.csv")

library(cluster)

clusterData <- data.matrix(x2MDall[,c("P12_1", "P12_2")])
plot(clusterData+1e-8, log="xy")

fit <- kmeans(clusterData, 10)

# removing outlier clusters (those with one representative only). 
singleClusters <- which(fit$size<=3)
clusterDataCleaned1 <- clusterData[! (fit$cluster %in% singleClusters),]

# plotting cleaned cluster.
plot(clusterDataCleaned1+1e-8, log="xy")

# simple k-means
fit <- kmeans(clusterDataCleaned1, 5)
clusplot(clusterDataCleaned1, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

### other cluster methods... 
library(fpc)
plotcluster(clusterDataCleaned1, fit$cluster)

library(mclust)
fit <- Mclust(clusterDataCleaned1)
plot(fit) # plot results
summary(fit) # display the best model 

#######################################
#######################################
x4ModelData <- read.csv("x4ModelsOptParams_2019_0114.csv")
x4MDKL <- read.csv("x4ModelsKLDivs_2019_0114.csv")

names(x4ModelData) <- c("X", "V1", "P1","P2","P3","P23_2","P23_3","P13_1","P13_3","P12_1","P12_2","P123_1","P123_2","P123_3")
names(x4MDKL) <- c("X","V1","KL_Base","KL_1","KL_2","KL_3","KL_23","KL_13","KL_12","KL_123")

x4MDall <- merge(x4ModelData, x4MDKL, by.x = "V1", by.y = "V1")
x4MDall <- x4MDall[which(colnames(x4MDall)!="X.x")]
x4MDall <- x4MDall[which(colnames(x4MDall)!="X.y")]

write.csv(x4MDall, "x4ModelValuesAll_2019_0114.csv")

library(cluster)

clusterData <- data.matrix(x4MDall[,c("P12_1", "P12_2")])

fit <- kmeans(clusterData, 10)

# removing outlier clusters (those with one representative only). 
singleClusters <- which(fit$size==1)
clusterDataCleaned1 <- clusterData[! (fit$cluster %in% singleClusters),]

# plotting cleaned cluster.
plot(clusterDataCleaned1+1e-8, log="xy")


