source("CommonCode/getConstCodeStratUtt.R")

# TODO 
# ... this is taken from X6 and only partially adapted to X8
# ... still needs to be adapted such that it focuses on the relevant feature

#x8pilotData <- read.csv("X8_Data/x8pilotDataAugm_UttChoice_SRSA_fixedAndD.csv")
#x8pilotData <- read.csv("X8_Data/x8pilotDataAugm_UttChoice_SRSA_ADandBD.csv")
x8pilotData <- read.csv("X8_Data/x8pilotDataAugm_UttChoice_SRSA_ABandABD.csv")

## adding the 1-27 target and object2 & object3 code.
o1 <- x8pilotData$obj1OC27
o2 <- x8pilotData$obj2OC27
o3 <- x8pilotData$obj3OC27
##

########### 
## filtering for only the present feature values for each feature.
###########
subjectGuessIndexM1 <- grep("^DPost_1", colnames(x8pilotData)) - 1
modelGuessIndex1M1 <- grep("^MPost1_1", colnames(x8pilotData)) - 1
modelGuessIndex2M1 <- grep("^MPost2_1", colnames(x8pilotData)) - 1
for(i in c(1:nrow(x8pilotData))) {
  currentObjects <- c(o1[i], o2[i], o3[i])
  validUtterances <- determineValidUtterances(currentObjects)
  for(j in c(1:3)) { # iterating over the three feature types
    relevantIndices <- which(validUtterances>(3*(j-1)) & validUtterances<(3*j + 1)) # relevant indices for a particular feature type
    valUttRel <- validUtterances[relevantIndices]
    # setting the non-represented values to NA
    for(v in c(1:3)) {
      if(length(which(valUttRel == ((j-1)*3) + v )) == 0) {
        x8pilotData[[subjectGuessIndexM1 + ((j-1)*3) + v]][i] <- NA
        x8pilotData[[modelGuessIndex1M1 + ((j-1)*3) + v]][i] <- NA
        x8pilotData[[modelGuessIndex2M1 + ((j-1)*3) + v]][i] <- NA
      }
    }
  }
}


## now determining the constellation code. 
uniqueCCode <- rep(0, length(x8pilotData$X))
featureOrder <- matrix(0, length(x8pilotData$X), 3)
objectOrder <- matrix(0, length(x8pilotData$X), 3)
featureValueOrder <- list()
for(i in c(1:length(x8pilotData$X))) {
  objectConstellation <- c(o1[i],o2[i],o3[i])
  cc <- getUtteranceChoiceConstellationCode(objectConstellation)
  uniqueCCode[i] <- cc[[1]]
  featureOrder[i,] <- cc[[2]]
  objectOrder[i,] <- cc[[3]]
  featureValueOrder[[i]] <- cc[[4]]
}
# feature order specifies reordering of standard order (e.g. shape=1, texture=2, color=3)
# object order specifies reordering of presented object order
# featureValueOrder specifies how the present feature in an object constellation should be ordered.
subjectGuessIndex <- grep("^DPost_1", colnames(x8pilotData))
modelGuessIndex1 <- grep("^MPost1_1", colnames(x8pilotData))
modelGuessIndex2 <- grep("^MPost2_1", colnames(x8pilotData))

x8pilotData$CCode <- uniqueCCode
x8pilotData$featValOrder <- featureValueOrder

modelDataOrdered <- matrix(-1,nrow(x8pilotData),16)
for(i in c(1:length(x8pilotData$X))) {
  for(j in c(1:length(x8pilotData$featValOrder[[i]]))) {
      modelDataOrdered[i,j] <- x8pilotData[[modelGuessIndex2M1+x8pilotData$featValOrder[[i]][j]]][i] 
  }
}
modelDataOrdered[,10] <- uniqueCCode
modelDataOrdered[,11] <- x8pilotData$obj1
modelDataOrdered[,12] <- x8pilotData$obj2
modelDataOrdered[,13] <- x8pilotData$obj3
modelDataOrdered[,14] <- x8pilotData$obj1OC27
modelDataOrdered[,15] <- x8pilotData$obj2OC27
modelDataOrdered[,16] <- x8pilotData$obj3OC27
write.csv(modelDataOrdered, "X8_Data/x8pilotDataModelOptimizedSorted.csv")
################################################################################

x8pilotData <- x8pilotData[order(x8pilotData$CCode),]
myCCodes <- unique(x8pilotData$CCode)
avDataMatrix <- matrix(0,length(myCCodes),19)
dataPointIndex <- 0
workerData <- 0
rsaModel1 <- 0
rsaModel2 <- 0
runIndex <- 1
for(i in c(1:length(myCCodes))) {
  cc <- myCCodes[i]
  cases <- which(x8pilotData$CCode == cc)
  allPilotDataCases <- x8pilotData[cases,]
  workerMeans <- 0
  rsaModel1Means <- 0
  rsaModel2Means <- 0
  for(j in c(1:nrow(allPilotDataCases))) {
    workerMeans <-  workerMeans + allPilotDataCases[j,subjectGuessIndexM1+(allPilotDataCases$featValOrder[[j]])]
    rsaModel1Means <- rsaModel1Means + allPilotDataCases[j,modelGuessIndex1M1+(allPilotDataCases$featValOrder[[j]])]
    rsaModel2Means <- rsaModel2Means + allPilotDataCases[j,modelGuessIndex2M1+(allPilotDataCases$featValOrder[[j]])]
  }
  for(j in c(1:length(workerMeans))) {
    workerData[runIndex] <- workerMeans[[j]] / nrow(allPilotDataCases)
    rsaModel1[runIndex] <- rsaModel1Means[[j]] / nrow(allPilotDataCases)
    rsaModel2[runIndex] <- rsaModel2Means[[j]] / nrow(allPilotDataCases)
    runIndex <- runIndex+1
  }
}
  
### plot after Optimization ###
rsaModel1 <- as.array(rsaModel1)

plot(rsaModel1, workerData)
abline(lm(formula = rsaModel1~workerData), col="red") # regression line (y~x)
lines(lowess(rsaModel1,workerData), col="blue") # lowess line (x,y)

model <- lm(formula = rsaModel1~workerData)
summary(model)
confint(model)

### plot with default parameters ###
rsaModel2 <- as.array(rsaModel2)

plot(rsaModel2, workerData)
abline(lm(formula = rsaModel2~workerData), col="red") # regression line (y~x)
lines(lowess(rsaModel2,workerData), col="blue") # lowess line (x,y)

model <- lm(formula = rsaModel2~workerData)
summary(model)
confint(model)




