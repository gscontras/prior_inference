source("CommonCode/getConstCodeStratUtt.R")

# loading the augmented pilot data (processed via RSA_x5UttChoiceDataProcessing01.R)
#x5UttChoiceData <- read.csv("X5_Data/x5pilotDataAugm_RSA_UttChoice_fixedAndD.csv")
#x5UttChoiceData <- read.csv("X5_Data/x5pilotDataAugm_RSA_UttChoice_ADandBD.csv")
#x5UttChoiceData <- read.csv("X5_Data/x5pilotDataAugm_RSA_UttChoice_ABandABD.csv")

#x5UttChoiceData <- read.csv("X5_Data/x5pilotDataAugm_UttChoice_SRSA_fixedAndD.csv")
x5UttChoiceData <- read.csv("X5_Data/x5pilotDataAugm_UttChoice_SRSA_ADandBD.csv")
#x5UttChoiceData <- read.csv("X5_Data/x5pilotDataAugm_UttChoice_SRSA_ABandABD.csv")
#x5UttChoiceData <- read.csv("X5_Data/x5pilotDataAugm_UttChoice_SRSA_BaseAndFixed000.csv")
#x5UttChoiceData <- x5UttChoiceData[which(x5UttChoiceData$trial_type=="utterance_choice"),]

## adding the 1-27 target and object2 & object3 code.
o1 <- x5UttChoiceData$obj1OC27
o2 <- x5UttChoiceData$obj2OC27
o3 <- x5UttChoiceData$obj3OC27
##

########### 
## filtering for only the present feature values for each feature.
###########
subjectGuessIndexM1 <- grep("^DPost_1", colnames(x5UttChoiceData)) - 1
modelGuessIndex1M1 <- grep("^MPost1_1", colnames(x5UttChoiceData)) - 1
modelGuessIndex2M1 <- grep("^MPost2_1", colnames(x5UttChoiceData)) - 1
for(i in c(1:nrow(x5UttChoiceData))) {
  currentObjects <- c(o1[i], o2[i], o3[i])
  validUtterances <- determineValidUtterances(currentObjects)
  for(j in c(1:3)) { # iterating over the three feature types
    relevantIndices <- which(validUtterances>(3*(j-1)) & validUtterances<(3*j + 1)) # relevant indices for a particular feature type
    valUttRel <- validUtterances[relevantIndices]
    # setting the non-represented values to NA
    for(v in c(1:3)) {
      if(length(which(valUttRel == ((j-1)*3) + v )) == 0) {
        x5UttChoiceData[[subjectGuessIndexM1 + ((j-1)*3) + v]][i] <- NA
        x5UttChoiceData[[modelGuessIndex1M1 + ((j-1)*3) + v]][i] <- NA
        x5UttChoiceData[[modelGuessIndex2M1 + ((j-1)*3) + v]][i] <- NA
      }
    }
  }
}


## now determining the constellation code. 
uniqueCCode <- rep(0, length(x5UttChoiceData$X))
featureOrder <- matrix(0, length(x5UttChoiceData$X), 3)
objectOrder <- matrix(0, length(x5UttChoiceData$X), 3)
featureValueOrder <- list()
for(i in c(1:length(x5UttChoiceData$X))) {
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
subjectGuessIndex <- grep("^DPost_1", colnames(x5UttChoiceData))
modelGuessIndex1 <- grep("^MPost1_1", colnames(x5UttChoiceData))
modelGuessIndex2 <- grep("^MPost2_1", colnames(x5UttChoiceData))

x5UttChoiceData$CCode <- uniqueCCode
x5UttChoiceData$featValOrder <- featureValueOrder
#write.csv(x5UttChoiceData, "x5UttChoiceDataModelOptimizedSorted.csv")
################################################################################


x5UttChoiceData <- x5UttChoiceData[order(x5UttChoiceData$CCode),]
myCCodes <- unique(x5UttChoiceData$CCode)
avDataMatrix <- matrix(0,length(myCCodes),19)
dataPointIndex <- 0
workerData <- 0
rsaModel1 <- 0
rsaModel2 <- 0
runIndex <- 1
for(i in c(1:length(myCCodes))) {
  cc <- myCCodes[i]
  cases <- which(x5UttChoiceData$CCode == cc)
  allPilotDataCases <- x5UttChoiceData[cases,]
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

plot(rsaModel2, workerData)
abline(lm(formula = rsaModel2~workerData), col="red") # regression line (y~x)
lines(lowess(rsaModel2,workerData), col="blue") # lowess line (x,y)

model <- lm(formula = rsaModel2~workerData)
summary(model)
confint(model)




