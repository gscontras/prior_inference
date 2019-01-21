source("RSA_StratUttModel_2019_0114.R")
source("RSA_StratUtt_getConstCode_2019_0114.R")

# loading the augmented pilot data (processed via RSA_x4PilotDataProcessing01.R)
x4pilotData <- read.csv("x4pilotDataAugmPar1and2_2019_0114.csv")

# adding feature property codes (which feature was uttereed, which features were questioned)
uttFeat <- ifelse(x4pilotData$utterance=="green" | x4pilotData$utterance=="red" | x4pilotData$utterance=="blue", 3,
                  ifelse(x4pilotData$utterance=="solid" | x4pilotData$utterance=="striped" | x4pilotData$utterance=="polka-dotted", 2, 1))
x4pilotData$uttFeat <- uttFeat

q1Feat <- ifelse(x4pilotData$pref1=="green things" | x4pilotData$pref1=="red things" | x4pilotData$pref1=="blue things", 3,
                 ifelse(x4pilotData$pref1=="solid things" | x4pilotData$pref1=="striped things" | x4pilotData$pref1=="polka-dotted things", 2, 
                        ifelse(x4pilotData$pref1=="clouds" | x4pilotData$pref1=="circles" | x4pilotData$pref1=="squares", 1,
                               -1 ) ))
x4pilotData$q1Feat <- q1Feat

q2Feat <- ifelse(x4pilotData$pref4=="green things" | x4pilotData$pref4=="red things" | x4pilotData$pref4=="blue things", 3,
                 ifelse(x4pilotData$pref4=="solid things" | x4pilotData$pref4=="striped things" | x4pilotData$pref4=="polka-dotted things", 2, 
                        ifelse(x4pilotData$pref4=="clouds" | x4pilotData$pref4=="circles" | x4pilotData$pref4=="squares", 1,
                               -1 ) ))
x4pilotData$q2Feat <- q2Feat

## adding the 1-27 target and object2 & object3 code.
temp <- x4pilotData$target
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
targetOC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x4pilotData$targetOC27 <- targetOC27

temp <- x4pilotData$obj2
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj2OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x4pilotData$obj2OC27 <- obj2OC27

temp <- x4pilotData$obj3
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj3OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x4pilotData$obj3OC27 <- obj3OC27


#### 
## filtering for only the present feature values for each feature.
###########
subjectGuessIndexM1 <- grep("^DataPost_1", colnames(x4pilotData)) - 1
modelGuessIndexM1 <- grep("^Post1_1", colnames(x4pilotData)) - 1
modelGuessIndexM2 <- grep("^Post2_1", colnames(x4pilotData)) - 1
for(i in c(1:nrow(x4pilotData))) {
  currentObjects <- c(targetOC27[i], obj2OC27[i], obj3OC27[i])
  validUtterances <- determineValidUtterances(currentObjects)
  for(j in c(1:3)) { # iterating over the three feature types
    relevantIndices <- which(validUtterances>(3*(j-1)) & validUtterances<(3*j + 1)) # relevant indices for a particular feature type
    valUttRel <- validUtterances[relevantIndices]
    sumSG <- 0
    sumMG <- 0
    sumMG2 <- 0
    for(x in c(1:length(valUttRel))) {
      sumSG <- sumSG + x4pilotData[[valUttRel[x]+subjectGuessIndexM1]][i]
      sumMG <- sumMG + x4pilotData[[valUttRel[x]+modelGuessIndexM1]][i]
      sumMG2 <- sumMG2 + x4pilotData[[valUttRel[x]+modelGuessIndexM2]][i]
    }
    if(!is.na(sumSG)) {
      for(x in c(1:length(valUttRel))) {
        x4pilotData[[valUttRel[x]+subjectGuessIndexM1]][i] <- x4pilotData[[valUttRel[x]+subjectGuessIndexM1]][i] /
          (sumSG + 1e-100)
      }
    }
    for(x in c(1:length(valUttRel))) {
      x4pilotData[[valUttRel[x]+modelGuessIndexM1]][i] <- x4pilotData[[valUttRel[x]+modelGuessIndexM1]][i] /
        (sumMG + 1e-100)
      x4pilotData[[valUttRel[x]+modelGuessIndexM2]][i] <- x4pilotData[[valUttRel[x]+modelGuessIndexM2]][i] /
        (sumMG2 + 1e-100)
    }
    # setting the non-represented values to NA
    for(v in c(1:3)) {
      if(length(which(valUttRel == ((j-1)*3) + v )) == 0) {
        x4pilotData[[subjectGuessIndexM1 + ((j-1)*3) + v]][i] <- NA
        x4pilotData[[modelGuessIndexM1 + ((j-1)*3) + v]][i] <- NA
        x4pilotData[[modelGuessIndexM2 + ((j-1)*3) + v]][i] <- NA
      }
    }
  }
}


## now determining the constellation code. 
constellationCode <- matrix(0,length(x4pilotData$X),6)
uniqueCCode <- rep(0, length(x4pilotData$X))
featureOrder <- matrix(0, length(x4pilotData$X), 3)
objectOrder <- matrix(0, length(x4pilotData$X), 3)
for(i in c(1:length(x4pilotData$X))) {
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
subjectGuessIndex <- grep("^DataPost_1", colnames(x4pilotData))
modelGuessIndex1 <- grep("^Post1_1", colnames(x4pilotData))
modelGuessIndex4 <- grep("^Post2_1", colnames(x4pilotData))

for(i in c(1:length(x4pilotData$X))) {
  # reordering the feature order
  x4pilotData[i,] <- replace(x4pilotData[i,], c(subjectGuessIndex:(subjectGuessIndex+8)),  
                           x4pilotData[i, c( (subjectGuessIndex + (featureOrder[i,1]-1)*3) : (subjectGuessIndex+2+(featureOrder[i,1]-1)*3),
                                           (subjectGuessIndex+ (featureOrder[i,2]-1)*3) : (subjectGuessIndex+2+(featureOrder[i,2]-1)*3),
                                           (subjectGuessIndex+ (featureOrder[i,3]-1)*3) : (subjectGuessIndex+2+(featureOrder[i,3]-1)*3) )])
  x4pilotData[i,] <- replace(x4pilotData[i,], c(modelGuessIndex1:(modelGuessIndex1+8)),  
                             x4pilotData[i, c( (modelGuessIndex1 + (featureOrder[i,1]-1)*3) : (modelGuessIndex1+2+(featureOrder[i,1]-1)*3),
                                               (modelGuessIndex1+ (featureOrder[i,2]-1)*3) : (modelGuessIndex1+2+(featureOrder[i,2]-1)*3),
                                               (modelGuessIndex1+ (featureOrder[i,3]-1)*3) : (modelGuessIndex1+2+(featureOrder[i,3]-1)*3) )])
  x4pilotData[i,] <- replace(x4pilotData[i,], c(modelGuessIndex4:(modelGuessIndex4+8)),  
                             x4pilotData[i, c( (modelGuessIndex4 + (featureOrder[i,1]-1)*3) : (modelGuessIndex4+2+(featureOrder[i,1]-1)*3),
                                               (modelGuessIndex4+ (featureOrder[i,2]-1)*3) : (modelGuessIndex4+2+(featureOrder[i,2]-1)*3),
                                               (modelGuessIndex4+ (featureOrder[i,3]-1)*3) : (modelGuessIndex4+2+(featureOrder[i,3]-1)*3) )])
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
    x4pilotData[i,] <- replace(x4pilotData[i,], c(((j-1)*3 + subjectGuessIndex):(2+((j-1)*3 + subjectGuessIndex))),  
                             x4pilotData[i, subjectGuessIndex + ((j-1)*3 + featValOrder)]) 
    x4pilotData[i,] <- replace(x4pilotData[i,], c(((j-1)*3 + modelGuessIndex1):(2+((j-1)*3 + modelGuessIndex1))),  
                               x4pilotData[i, modelGuessIndex1 + ((j-1)*3 + featValOrder)]) 
    x4pilotData[i,] <- replace(x4pilotData[i,], c(((j-1)*3 + modelGuessIndex4):(2+((j-1)*3 + modelGuessIndex4))),  
                               x4pilotData[i, modelGuessIndex4 + ((j-1)*3 + featValOrder)]) 
  }
}
x4pilotData$CCode <- uniqueCCode
#write.csv(x4pilotData, "x4pilotDataModelOptimizedSorted.csv")

x4pilotData <- x4pilotData[order(x4pilotData$CCode),]
myCCodes <- unique(x4pilotData$CCode)
avDataMatrix <- matrix(0,length(myCCodes),19)
dataPointIndex <- 0
workerData <- 0
rsaModel <- 0
rsaModel2 <- 0
for(i in c(1:length(myCCodes))) {
  cc <- myCCodes[i]
  cases <- which(x4pilotData$CCode == cc)
  allPilotDataCases <- x4pilotData[cases,]
  for(j in c(1:9)) {
    specCases <- which(is.na(allPilotDataCases[,subjectGuessIndex-1+j]) == FALSE)
    if(length(specCases) > 0) {
#      if(mean(allPilotDataCases[specCases,(modelGuessIndex4-1+j)]) < 1/3 - 1e-5
#         | mean(allPilotDataCases[specCases,(modelGuessIndex4-1+j)]) > 1/3 + 1e-5) {
        dataPointIndex <- dataPointIndex + 1
        workerData[dataPointIndex] <- mean(allPilotDataCases[specCases,(subjectGuessIndex-1+j)])
        rsaModel[dataPointIndex] <- mean(allPilotDataCases[specCases,(modelGuessIndex1-1+j)])
        rsaModel2[dataPointIndex] <- mean(allPilotDataCases[specCases,(modelGuessIndex4-1+j)])
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




