source("RSA_StratUttModel_2019_0114.R")
source("RSA_StratUtt_getConstCode_2019_0114.R")

# loading the augmented pilot data (processed via RSA_x5PilotDataProcessing01.R)
#x5pilotData <- read.csv("x5priorInfDataAugmPar1and2.csv")
#x5pilotData <- read.csv("x5priorInfDataAugm_SimpleRSA_P1or2_2019_05_17.csv")
x5pilotData <- read.csv("x5priorInfDataAugm_SimpleRSA_P1no.1or.2_2019_05_17.csv")
#x5pilotData <- read.csv("x5priorInfDataAugm_SimpleRSA_P2pref.2andP12_2019_05_17.csv")

# adding feature property codes (which feature was uttereed, which features were questioned)
uttFeat <- ifelse(x5pilotData$utterance=="green" | x5pilotData$utterance=="red" | x5pilotData$utterance=="blue", 3,
                  ifelse(x5pilotData$utterance=="solid" | x5pilotData$utterance=="striped" | x5pilotData$utterance=="polka-dotted", 2, 1))
x5pilotData$uttFeat <- uttFeat

q1Feat <- ifelse(x5pilotData$pref1=="green things" | x5pilotData$pref1=="red things" | x5pilotData$pref1=="blue things", 3,
                 ifelse(x5pilotData$pref1=="solid things" | x5pilotData$pref1=="striped things" | x5pilotData$pref1=="polka-dotted things", 2, 
                        ifelse(x5pilotData$pref1=="clouds" | x5pilotData$pref1=="circles" | x5pilotData$pref1=="squares", 1,
                               -1 ) ))
x5pilotData$q1Feat <- q1Feat

q2Feat <- ifelse(x5pilotData$pref4=="green things" | x5pilotData$pref4=="red things" | x5pilotData$pref4=="blue things", 3,
                 ifelse(x5pilotData$pref4=="solid things" | x5pilotData$pref4=="striped things" | x5pilotData$pref4=="polka-dotted things", 2, 
                        ifelse(x5pilotData$pref4=="clouds" | x5pilotData$pref4=="circles" | x5pilotData$pref4=="squares", 1,
                               -1 ) ))
x5pilotData$q2Feat <- q2Feat

## adding the 1-27 target and object2 & object3 code.
temp <- x5pilotData$target
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
targetOC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x5pilotData$targetOC27 <- targetOC27

temp <- x5pilotData$obj2
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj2OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x5pilotData$obj2OC27 <- obj2OC27

temp <- x5pilotData$obj3
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj3OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x5pilotData$obj3OC27 <- obj3OC27


#### 
## filtering for only the present feature values for each feature.
###########
subjectGuessIndexM1 <- grep("^DataPost_1", colnames(x5pilotData)) - 1
modelGuessIndexM1 <- grep("^Post1_1", colnames(x5pilotData)) - 1
modelGuessIndexM2 <- grep("^Post2_1", colnames(x5pilotData)) - 1
for(i in c(1:nrow(x5pilotData))) {
  currentObjects <- c(targetOC27[i], obj2OC27[i], obj3OC27[i])
  validUtterances <- determineValidUtterances(currentObjects)
  for(j in c(1:3)) { # iterating over the three feature types
    relevantIndices <- which(validUtterances>(3*(j-1)) & validUtterances<(3*j + 1)) # relevant indices for a particular feature type
    valUttRel <- validUtterances[relevantIndices]
    sumSG <- 0
    sumMG <- 0
    sumMG2 <- 0
    for(x in c(1:length(valUttRel))) {
      sumSG <- sumSG + x5pilotData[[valUttRel[x]+subjectGuessIndexM1]][i]
      sumMG <- sumMG + x5pilotData[[valUttRel[x]+modelGuessIndexM1]][i]
      sumMG2 <- sumMG2 + x5pilotData[[valUttRel[x]+modelGuessIndexM2]][i]
    }
    if(!is.na(sumSG)) {
      for(x in c(1:length(valUttRel))) {
        x5pilotData[[valUttRel[x]+subjectGuessIndexM1]][i] <- x5pilotData[[valUttRel[x]+subjectGuessIndexM1]][i] /
          (sumSG + 1e-100)
      }
    }
    for(x in c(1:length(valUttRel))) {
      x5pilotData[[valUttRel[x]+modelGuessIndexM1]][i] <- x5pilotData[[valUttRel[x]+modelGuessIndexM1]][i] /
        (sumMG + 1e-100)
      x5pilotData[[valUttRel[x]+modelGuessIndexM2]][i] <- x5pilotData[[valUttRel[x]+modelGuessIndexM2]][i] /
        (sumMG2 + 1e-100)
    }
    # setting the non-represented values to NA
    for(v in c(1:3)) {
      if(length(which(valUttRel == ((j-1)*3) + v )) == 0) {
        x5pilotData[[subjectGuessIndexM1 + ((j-1)*3) + v]][i] <- NA
        x5pilotData[[modelGuessIndexM1 + ((j-1)*3) + v]][i] <- NA
        x5pilotData[[modelGuessIndexM2 + ((j-1)*3) + v]][i] <- NA
      }
    }
  }
}


## now determining the constellation code. 
constellationCode <- matrix(0,length(x5pilotData$X),6)
uniqueCCode <- rep(0, length(x5pilotData$X))
featureOrder <- matrix(0, length(x5pilotData$X), 3)
objectOrder <- matrix(0, length(x5pilotData$X), 3)
for(i in c(1:length(x5pilotData$X))) {
  objectConstellation <- c(targetOC27[i],obj2OC27[i],obj3OC27[i])
  featChoice <- uttFeat[i]
  cc <- getConstellationCode(objectConstellation, featChoice)
  constellationCode[i,] <- cc[[1]]
  featureOrder[i,] <- cc[[2]]
  objectOrder[i,] <- cc[[3]]
  ## one-column code
  uc <- 0
  for(j in c(1:6)) {
    uc <- (uc * 10) + constellationCode[i,j]
  }
  uniqueCCode[i] <- uc
}
# feature order specified reordering of standard order (i.e. shape=1, texture=2, color=3)
# object order specifies reordering of presented object order
subjectGuessIndex <- grep("^DataPost_1", colnames(x5pilotData))
modelGuessIndex1 <- grep("^Post1_1", colnames(x5pilotData))
modelGuessIndex5 <- grep("^Post2_1", colnames(x5pilotData))

for(i in c(1:length(x5pilotData$X))) {
  # reordering the feature order
  x5pilotData[i,] <- replace(x5pilotData[i,], c(subjectGuessIndex:(subjectGuessIndex+8)),  
                           x5pilotData[i, c( (subjectGuessIndex + (featureOrder[i,1]-1)*3) : (subjectGuessIndex+2+(featureOrder[i,1]-1)*3),
                                           (subjectGuessIndex+ (featureOrder[i,2]-1)*3) : (subjectGuessIndex+2+(featureOrder[i,2]-1)*3),
                                           (subjectGuessIndex+ (featureOrder[i,3]-1)*3) : (subjectGuessIndex+2+(featureOrder[i,3]-1)*3) )])
  x5pilotData[i,] <- replace(x5pilotData[i,], c(modelGuessIndex1:(modelGuessIndex1+8)),  
                             x5pilotData[i, c( (modelGuessIndex1 + (featureOrder[i,1]-1)*3) : (modelGuessIndex1+2+(featureOrder[i,1]-1)*3),
                                               (modelGuessIndex1+ (featureOrder[i,2]-1)*3) : (modelGuessIndex1+2+(featureOrder[i,2]-1)*3),
                                               (modelGuessIndex1+ (featureOrder[i,3]-1)*3) : (modelGuessIndex1+2+(featureOrder[i,3]-1)*3) )])
  x5pilotData[i,] <- replace(x5pilotData[i,], c(modelGuessIndex5:(modelGuessIndex5+8)),  
                             x5pilotData[i, c( (modelGuessIndex5 + (featureOrder[i,1]-1)*3) : (modelGuessIndex5+2+(featureOrder[i,1]-1)*3),
                                               (modelGuessIndex5+ (featureOrder[i,2]-1)*3) : (modelGuessIndex5+2+(featureOrder[i,2]-1)*3),
                                               (modelGuessIndex5+ (featureOrder[i,3]-1)*3) : (modelGuessIndex5+2+(featureOrder[i,3]-1)*3) )])
  ## now rearranging the individual feature values dependent on the object order (first object is the chosen one!)
  objectConstellation <- c(targetOC27[i],obj2OC27[i],obj3OC27[i])
  objectCReordered <- replace(objectConstellation, c(1:3), objectConstellation[objectOrder[i,]])

  for(j in c(1:3)) {
    featValOrder <- rep(0,3)
    targetFeatureValue <- allObjectsToUtterancesMappings[objectCReordered[1],featureOrder[i,j]]
    featValOrder[1] <-  targetFeatureValue # target feature value comes first
    featValIndex <- 2
    for(k in c(2:3)) {
      objectFeatureValue <- allObjectsToUtterancesMappings[objectCReordered[k],featureOrder[i,j]]
      if(length(which(featValOrder==objectFeatureValue))==0) { # feature not included yet
        featValOrder[featValIndex] <- objectFeatureValue
        featValIndex <- featValIndex + 1
      }
    }
    if(featValIndex < 4) { # not all feature values assigned, yet -> fill up
      for(featVal in c(((featureOrder[i,j]-1)*3+1):((featureOrder[i,j]-1)*3+3))) {
        if(length(which(featValOrder==featVal))==0) { # feature not included yet
          featValOrder[featValIndex] <- featVal
          featValIndex <- featValIndex + 1
        }
      }
    }
    featValOrder <- featValOrder - (featureOrder[i,j]-1)*3 - 1
    ### now featValOrder specifies the feature value reordering for order feature with index j
    # reordering the feature value order of ordered feature j 
    x5pilotData[i,] <- replace(x5pilotData[i,], c(((j-1)*3 + subjectGuessIndex):(2+((j-1)*3 + subjectGuessIndex))),  
                             x5pilotData[i, subjectGuessIndex + ((j-1)*3 + featValOrder)]) 
    x5pilotData[i,] <- replace(x5pilotData[i,], c(((j-1)*3 + modelGuessIndex1):(2+((j-1)*3 + modelGuessIndex1))),  
                               x5pilotData[i, modelGuessIndex1 + ((j-1)*3 + featValOrder)]) 
    x5pilotData[i,] <- replace(x5pilotData[i,], c(((j-1)*3 + modelGuessIndex5):(2+((j-1)*3 + modelGuessIndex5))),  
                               x5pilotData[i, modelGuessIndex5 + ((j-1)*3 + featValOrder)]) 
  }
}
x5pilotData$CCode <- uniqueCCode
#write.csv(x5pilotData, "x5pilotDataModelOptimizedSorted.csv")

x5pilotData <- x5pilotData[order(x5pilotData$CCode),]
myCCodes <- unique(x5pilotData$CCode)
avDataMatrix <- matrix(0,length(myCCodes),19)
dataPointIndex <- 0
workerData <- 0
rsaModel <- 0
rsaModel2 <- 0
for(i in c(1:length(myCCodes))) {
  cc <- myCCodes[i]
  cases <- which(x5pilotData$CCode == cc)
  allPilotDataCases <- x5pilotData[cases,]
  for(j in c(1:9)) {
    specCases <- which(is.na(allPilotDataCases[,subjectGuessIndex-1+j]) == FALSE)
    if(length(specCases) > 0) {
#      if(mean(allPilotDataCases[specCases,(modelGuessIndex5-1+j)]) < 1/3 - 1e-5
#         | mean(allPilotDataCases[specCases,(modelGuessIndex5-1+j)]) > 1/3 + 1e-5) {
        dataPointIndex <- dataPointIndex + 1
        workerData[dataPointIndex] <- mean(allPilotDataCases[specCases,(subjectGuessIndex-1+j)])
        rsaModel[dataPointIndex] <- mean(allPilotDataCases[specCases,(modelGuessIndex1-1+j)])
        rsaModel2[dataPointIndex] <- mean(allPilotDataCases[specCases,(modelGuessIndex5-1+j)])
#      }
    }
  }
}
### plot after Optimization ###


plot(rsaModel, workerData)
abline(lm(formula = rsaModel~workerData), col="red") # regression line (y~x)
lines(lowess(rsaModel,workerData), col="blue") # lowess line (x,y)

model <- lm(formula = rsaModel~workerData)
summary(model)
confint(model)

### plot with default parameters ###

plot(rsaModel2, workerData)
abline(lm(formula = rsaModel2~workerData), col="red") # regression line (y~x)
lines(lowess(rsaModel2,workerData), col="blue") # lowess line (x,y)

model <- lm(formula = rsaModel2~workerData)
summary(model)
confint(model)




