source("RSA_StratUttModel_2019_0114.R")
source("RSA_StratUtt_getConstCode_2019_0114.R")

# loading the augmented pilot data (processed via RSA_x5DataAugmProcessing01.R)
x5DataAugm <- read.csv("x5DataAugmentedUttChoicePriorInfPO.csv")

## adding the 1-27 target and object2 & object3 code.
o1 <- x5DataAugm$obj1OC27
o2 <- x5DataAugm$obj2OC27
o3 <- x5DataAugm$obj3OC27
##

########### 
## filtering for only the present feature values for each feature.
###########
subjectGuessIndexM1 <- grep("^DPost_1", colnames(x5DataAugm)) - 1
modelGuessIndex0M1 <- grep("^MPostNO_1", colnames(x5DataAugm)) - 1
modelGuessIndex1M1 <- grep("^MPost1_1", colnames(x5DataAugm)) - 1
modelGuessIndex2M1 <- grep("^MPost2_1", colnames(x5DataAugm)) - 1
modelGuessIndex3M1 <- grep("^MPost3_1", colnames(x5DataAugm)) - 1
for(i in c(1:nrow(x5DataAugm))) {
  currentObjects <- c(o1[i], o2[i], o3[i])
  validUtterances <- determineValidUtterances(currentObjects)
  for(j in c(1:3)) { # iterating over the three feature types
    relevantIndices <- which(validUtterances>(3*(j-1)) & validUtterances<(3*j + 1)) # relevant indices for a particular feature type
    valUttRel <- validUtterances[relevantIndices]
    # setting the non-represented values to NA
    for(v in c(1:3)) {
      if(length(which(valUttRel == ((j-1)*3) + v )) == 0) {
        x5DataAugm[[subjectGuessIndexM1 + ((j-1)*3) + v]][i] <- NA
        x5DataAugm[[modelGuessIndex0M1 + ((j-1)*3) + v]][i] <- NA
        x5DataAugm[[modelGuessIndex1M1 + ((j-1)*3) + v]][i] <- NA
        x5DataAugm[[modelGuessIndex2M1 + ((j-1)*3) + v]][i] <- NA
        x5DataAugm[[modelGuessIndex3M1 + ((j-1)*3) + v]][i] <- NA
      }
    }
  }
}


## now determining the constellation code. 
uniqueCCode <- rep(0, length(x5DataAugm$X))
featureOrder <- matrix(0, length(x5DataAugm$X), 3)
objectOrder <- matrix(0, length(x5DataAugm$X), 3)
featureValueOrder <- list()
for(i in c(1:length(x5DataAugm$X))) {
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
subjectGuessIndex <- grep("^DPost_1", colnames(x5DataAugm))
modelGuessIndex0 <- grep("^MPostNO_1", colnames(x5DataAugm))
modelGuessIndex1 <- grep("^MPost1_1", colnames(x5DataAugm))
modelGuessIndex2 <- grep("^MPost2_1", colnames(x5DataAugm))
modelGuessIndex3 <- grep("^MPost3_1", colnames(x5DataAugm))

x5DataAugm$CCode <- uniqueCCode
x5DataAugm$featValOrder <- featureValueOrder
#write.csv(x5DataAugm, "x5DataAugmModelOptimizedSorted.csv")
################################################################################


x5DataAugm <- x5DataAugm[order(x5DataAugm$CCode),]
myCCodes <- unique(x5DataAugm$CCode)
avDataMatrix <- matrix(0,length(myCCodes),19)
dataPointIndex <- 0
workerData <- 0
rsaModel0 <- 0
rsaModel1 <- 0
rsaModel2 <- 0
rsaModel3 <- 0
runIndex <- 1
for(i in c(1:length(myCCodes))) {
  cc <- myCCodes[i]
  cases <- which(x5DataAugm$CCode == cc)
  allPilotDataCases <- x5DataAugm[cases,]
  workerMeans <- 0
  rsaModel0Means <- 0
  rsaModel1Means <- 0
  rsaModel2Means <- 0
  rsaModel3Means <- 0
  for(j in c(i:nrow(allPilotDataCases))) {
    workerMeans <-  workerMeans + allPilotDataCases[j,subjectGuessIndexM1+(allPilotDataCases$featValOrder[[j]])]
    rsaModel0Means <- rsaModel0Means + allPilotDataCases[j,modelGuessIndex0M1+(allPilotDataCases$featValOrder[[j]])]
    rsaModel1Means <- rsaModel1Means + allPilotDataCases[j,modelGuessIndex1M1+(allPilotDataCases$featValOrder[[j]])]
    rsaModel2Means <- rsaModel2Means + allPilotDataCases[j,modelGuessIndex2M1+(allPilotDataCases$featValOrder[[j]])]
    rsaModel3Means <- rsaModel3Means + allPilotDataCases[j,modelGuessIndex3M1+(allPilotDataCases$featValOrder[[j]])]
  }
  for(j in c(1:length(workerMeans))) {
    workerData[runIndex] <- workerMeans[[j]] / nrow(allPilotDataCases)
    rsaModel0[runIndex] <- rsaModel0Means[[j]] / nrow(allPilotDataCases)
    rsaModel1[runIndex] <- rsaModel1Means[[j]] / nrow(allPilotDataCases)
    rsaModel2[runIndex] <- rsaModel2Means[[j]] / nrow(allPilotDataCases)
    rsaModel3[runIndex] <- rsaModel3Means[[j]] / nrow(allPilotDataCases)
    runIndex <- runIndex+1
  }
}
  
### plot after Optimization -- default parameters ###
rsaModel0 <- as.array(rsaModel0)
plot(rsaModel0, workerData)
title(main = "All Default Parameters")
abline(lm(formula = rsaModel0~workerData), col="red") # regression line (y~x)
lines(lowess(rsaModel0,workerData), col="blue") # lowess line (x,y)

model <- lm(formula = rsaModel0~workerData)
summary(model)
confint(model)

### plot after Optimization -- only gamma and delta optimized ###
rsaModel1 <- as.array(rsaModel1)
plot(rsaModel1, workerData)
title(main = "Gamma Prior Inf Opt.; Delta Optimized")
abline(lm(formula = rsaModel1~workerData), col="red") # regression line (y~x)
lines(lowess(rsaModel1,workerData), col="blue") # lowess line (x,y)

model <- lm(formula = rsaModel1~workerData)
summary(model)
confint(model)

### plot after Optimization -- only beta, gamme, and delta optimized ###
rsaModel2 <- as.array(rsaModel2)
plot(rsaModel2, workerData)
title(main = "Beta and Gamma Prior Inf Opt.; Delta Optimized")
abline(lm(formula = rsaModel2~workerData), col="red") # regression line (y~x)
lines(lowess(rsaModel2,workerData), col="blue") # lowess line (x,y)

model <- lm(formula = rsaModel2~workerData)
summary(model)
confint(model)

### plot after Optimization -- all four parameters optimized ###
rsaModel3 <- as.array(rsaModel3)
plot(rsaModel3, workerData)
title(main = "Alpha, Beta, & Gamma Prior Inf Opt.; Delta Optimized")
abline(lm(formula = rsaModel3~workerData), col="red") # regression line (y~x)
lines(lowess(rsaModel3,workerData), col="blue") # lowess line (x,y)

model <- lm(formula = rsaModel3~workerData)
summary(model)
confint(model)







