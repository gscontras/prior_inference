source("RSA_StratUttModel_2019_0114.R")
source("RSA_StratUtt_getConstCode_2019_0114.R")

# loading the augmented pilot data (processed via RSA_x2PilotDataProcessing01.R)
x2pilotData <- read.csv("x2pilotDataAugmPar1and2_2019_0114.csv")

# adding feature property codes (which feature was uttereed, which features were questioned)
uttFeat <- ifelse(x2pilotData$utterance=="green" | x2pilotData$utterance=="red" | x2pilotData$utterance=="blue", 3,
                  ifelse(x2pilotData$utterance=="solid" | x2pilotData$utterance=="striped" | x2pilotData$utterance=="polka-dotted", 2, 1))
x2pilotData$uttFeat <- uttFeat

q1Feat <- ifelse(x2pilotData$pref1=="green things" | x2pilotData$pref1=="red things" | x2pilotData$pref1=="blue things", 3,
                 ifelse(x2pilotData$pref1=="solid things" | x2pilotData$pref1=="striped things" | x2pilotData$pref1=="polka-dotted things", 2, 
                        ifelse(x2pilotData$pref1=="clouds" | x2pilotData$pref1=="circles" | x2pilotData$pref1=="squares", 1,
                               -1 ) ))
x2pilotData$q1Feat <- q1Feat

q2Feat <- ifelse(x2pilotData$pref4=="green things" | x2pilotData$pref4=="red things" | x2pilotData$pref4=="blue things", 3,
                 ifelse(x2pilotData$pref4=="solid things" | x2pilotData$pref4=="striped things" | x2pilotData$pref4=="polka-dotted things", 2, 
                        ifelse(x2pilotData$pref4=="clouds" | x2pilotData$pref4=="circles" | x2pilotData$pref4=="squares", 1,
                               -1 ) ))
x2pilotData$q2Feat <- q2Feat

## adding the 1-27 target and object2 & object3 code.
temp <- x2pilotData$target
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
targetOC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x2pilotData$targetOC27 <- targetOC27

temp <- x2pilotData$obj2
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj2OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x2pilotData$obj2OC27 <- obj2OC27

temp <- x2pilotData$obj3
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj3OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x2pilotData$obj3OC27 <- obj3OC27


#### 
## filtering for only the present feature values for each feature.
###########
subjectGuessIndexM1 <- grep("^DataPost_1", colnames(x2pilotData)) - 1
modelGuessIndexM1 <- grep("^Post1_1", colnames(x2pilotData)) - 1
modelGuessIndexM2 <- grep("^Post2_1", colnames(x2pilotData)) - 1
for(i in c(1:nrow(x2pilotData))) {
  currentObjects <- c(targetOC27[i], obj2OC27[i], obj3OC27[i])
  validUtterances <- determineValidUtterances(currentObjects)
  for(j in c(1:3)) { # iterating over the three feature types
    relevantIndices <- which(validUtterances>(3*(j-1)) & validUtterances<(3*j + 1)) # relevant indices for a particular feature type
    valUttRel <- validUtterances[relevantIndices]
    sumSG <- 0
    sumMG <- 0
    sumMG2 <- 0
    for(x in c(1:length(valUttRel))) {
      sumSG <- sumSG + x2pilotData[[valUttRel[x]+subjectGuessIndexM1]][i]
      sumMG <- sumMG + x2pilotData[[valUttRel[x]+modelGuessIndexM1]][i]
      sumMG2 <- sumMG2 + x2pilotData[[valUttRel[x]+modelGuessIndexM2]][i]
    }
    if(!is.na(sumSG)) {
      for(x in c(1:length(valUttRel))) {
        x2pilotData[[valUttRel[x]+subjectGuessIndexM1]][i] <- x2pilotData[[valUttRel[x]+subjectGuessIndexM1]][i] /
          (sumSG + 1e-100)
      }
    }
    for(x in c(1:length(valUttRel))) {
      x2pilotData[[valUttRel[x]+modelGuessIndexM1]][i] <- x2pilotData[[valUttRel[x]+modelGuessIndexM1]][i] /
        (sumMG + 1e-100)
      x2pilotData[[valUttRel[x]+modelGuessIndexM2]][i] <- x2pilotData[[valUttRel[x]+modelGuessIndexM2]][i] /
        (sumMG2 + 1e-100)
    }
    # setting the non-represented values to NA
    for(v in c(1:3)) {
      if(length(which(valUttRel == ((j-1)*3) + v )) == 0) {
        x2pilotData[[subjectGuessIndexM1 + ((j-1)*3) + v]][i] <- NA
        x2pilotData[[modelGuessIndexM1 + ((j-1)*3) + v]][i] <- NA
        x2pilotData[[modelGuessIndexM2 + ((j-1)*3) + v]][i] <- NA
      }
    }
  }
}


## now determining the constellation code. 
constellationCode <- matrix(0,length(x2pilotData$X),6)
uniqueCCode <- rep(0, length(x2pilotData$X))
featureOrder <- matrix(0, length(x2pilotData$X), 3)
objectOrder <- matrix(0, length(x2pilotData$X), 3)
for(i in c(1:length(x2pilotData$X))) {
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
subjectGuessIndex <- grep("^DataPost_1", colnames(x2pilotData))
modelGuessIndex1 <- grep("^Post1_1", colnames(x2pilotData))
modelGuessIndex2 <- grep("^Post2_1", colnames(x2pilotData))

for(i in c(1:length(x2pilotData$X))) {
  # reordering the feature order
  x2pilotData[i,] <- replace(x2pilotData[i,], c(subjectGuessIndex:(subjectGuessIndex+8)),  
                           x2pilotData[i, c( (subjectGuessIndex + (featureOrder[i,1]-1)*3) : (subjectGuessIndex+2+(featureOrder[i,1]-1)*3),
                                           (subjectGuessIndex+ (featureOrder[i,2]-1)*3) : (subjectGuessIndex+2+(featureOrder[i,2]-1)*3),
                                           (subjectGuessIndex+ (featureOrder[i,3]-1)*3) : (subjectGuessIndex+2+(featureOrder[i,3]-1)*3) )])
  x2pilotData[i,] <- replace(x2pilotData[i,], c(modelGuessIndex1:(modelGuessIndex1+8)),  
                             x2pilotData[i, c( (modelGuessIndex1 + (featureOrder[i,1]-1)*3) : (modelGuessIndex1+2+(featureOrder[i,1]-1)*3),
                                               (modelGuessIndex1+ (featureOrder[i,2]-1)*3) : (modelGuessIndex1+2+(featureOrder[i,2]-1)*3),
                                               (modelGuessIndex1+ (featureOrder[i,3]-1)*3) : (modelGuessIndex1+2+(featureOrder[i,3]-1)*3) )])
  x2pilotData[i,] <- replace(x2pilotData[i,], c(modelGuessIndex2:(modelGuessIndex2+8)),  
                             x2pilotData[i, c( (modelGuessIndex2 + (featureOrder[i,1]-1)*3) : (modelGuessIndex2+2+(featureOrder[i,1]-1)*3),
                                               (modelGuessIndex2+ (featureOrder[i,2]-1)*3) : (modelGuessIndex2+2+(featureOrder[i,2]-1)*3),
                                               (modelGuessIndex2+ (featureOrder[i,3]-1)*3) : (modelGuessIndex2+2+(featureOrder[i,3]-1)*3) )])
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
    x2pilotData[i,] <- replace(x2pilotData[i,], c(((j-1)*3 + subjectGuessIndex):(2+((j-1)*3 + subjectGuessIndex))),  
                             x2pilotData[i, subjectGuessIndex + ((j-1)*3 + featValOrder)]) 
    x2pilotData[i,] <- replace(x2pilotData[i,], c(((j-1)*3 + modelGuessIndex1):(2+((j-1)*3 + modelGuessIndex1))),  
                               x2pilotData[i, modelGuessIndex1 + ((j-1)*3 + featValOrder)]) 
    x2pilotData[i,] <- replace(x2pilotData[i,], c(((j-1)*3 + modelGuessIndex2):(2+((j-1)*3 + modelGuessIndex2))),  
                               x2pilotData[i, modelGuessIndex2 + ((j-1)*3 + featValOrder)]) 
  }
}
x2pilotData$CCode <- uniqueCCode
#write.csv(x2pilotData, "x2pilotDataModelOptimizedSorted.csv")

x2pilotData <- x2pilotData[order(x2pilotData$CCode),]
myCCodes <- unique(x2pilotData$CCode)
avDataMatrix <- matrix(0,length(myCCodes),19)
dataPointIndex <- 0
workerData <- 0
rsaModel <- 0
rsaModel2 <- 0
for(i in c(1:length(myCCodes))) {
  cc <- myCCodes[i]
  cases <- which(x2pilotData$CCode == cc)
  allPilotDataCases <- x2pilotData[cases,]
  for(j in c(1:9)) {
    specCases <- which(is.na(allPilotDataCases[,subjectGuessIndex-1+j]) == FALSE)
    if(length(specCases) > 0) {
#      if(mean(allPilotDataCases[specCases,(modelGuessIndex2-1+j)]) < 1/3 - 1e-5
#         | mean(allPilotDataCases[specCases,(modelGuessIndex2-1+j)]) > 1/3 + 1e-5) {
        dataPointIndex <- dataPointIndex + 1
        workerData[dataPointIndex] <- mean(allPilotDataCases[specCases,(subjectGuessIndex-1+j)])
        rsaModel[dataPointIndex] <- mean(allPilotDataCases[specCases,(modelGuessIndex1-1+j)])
        rsaModel2[dataPointIndex] <- mean(allPilotDataCases[specCases,(modelGuessIndex2-1+j)])
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



### paper plot for optimized model
d = data.frame(rsaModel2,workerData)
require(ggplot2)
ggplot(d, aes(x=rsaModel2,y=workerData)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nmodel predictions")+
  ylab("human data\n")+
  theme_bw()
#ggsave("X2-scatter-CogSci.png",width=3,height=2.5)

### correlation analysis for paper
require(hydroGOF)
gof(as.numeric(d$rsaModel2),as.numeric(d$workerData)) ## r2 = 0.99
results <- boot(data=d, statistic=rsq, R=10000, formula=workerData~rsaModel2)
boot.ci(results, type="bca") # 95% CI  ( 0.9872,  0.9941 )  
