source("CommonCode/RSA_StratUtt.R")
source("CommonCode/RSA_UttChoiceOptimization.R")

# loading the raw pilot data (as Greg sent it on 2018/12/21)
x5pilotData <- read.csv("X5_Data/5-combined-unique.csv")
x5pilotData <- x5pilotData[which(x5pilotData$trial_type=="utterance_choice"),]

## adding the 1-27 target and object2 & object3 code.
temp <- x5pilotData$obj1
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj1OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x5pilotData$obj1OC27 <- obj1OC27

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

## now identify the first column number of the turker sliders and response pairs
sliderIndex <- grep("^pref1", colnames(x5pilotData))
## and use that index to determine all slider identities and corresponding slider values.
sliderUtteranceTypes <- matrix(NA, nrow(x5pilotData), 9)
sliderSetValues <- matrix(NA,  nrow(x5pilotData), 9)
for(i in c(1:9)) {
  colIndex <- sliderIndex + (i-1) * 2
  relRows <- which(!is.na(x5pilotData[[colIndex]]))
  for(j in c(1:length(relRows) ) ) { 
    sliderUtteranceTypes[relRows[j], i] <- which(allUtterancesNew1==x5pilotData[[colIndex]][relRows[j]])
    sliderSetValues[relRows[j], i] <- x5pilotData[[colIndex+1]][relRows[j]]
  }
}
### normalizing the turker estimates and setting them into the corresponding matrix.
bInfGainUttTurkers <- matrix(NA, nrow(x5pilotData), 9)
for(i in c(1:nrow(x5pilotData)) ) {
  s <- sum(sliderSetValues[i,c(1:x5pilotData$numFeatures[i])])
  if(s > 0) {
    sliderSetValues[i,c(1:x5pilotData$numFeatures[i])] <- sliderSetValues[i,c(1:x5pilotData$numFeatures[i])] / s
  }else{
    sliderSetValues[i,c(1:x5pilotData$numFeatures[i])] <- 1 / (x5pilotData$numFeatures[i])
  }
  bInfGainUttTurkers[i, sliderUtteranceTypes[i,c(1:(x5pilotData$numFeatures[i]) )] ] <- sliderSetValues[i,c(1:(x5pilotData$numFeatures[i]) )]
  for(j in c(1:x5pilotData$numFeatures[i])) {
    if(is.na(sliderSetValues[i,j])) {
      print("ERRor")
    }
  }
}

#pragmaticSpeaker <- function(utterance, obj, preferencesPrior, 
#                             relevantUtterances, currentObjects, mapUttToObjProbs,
#                             objectPreferenceSoftPriors, alpha) {
#bestInfGainUtterance <- function(preferencesPrior, relevantUtterances, currentObjects, 
#                                 mapUttToObjProbs, objectPreferenceSoftPriors, alpha) {

## reloading optimization values
#klDivUttWorkers <- as.matrix(read.csv("X5_Data/KLDivUttWorkers_x5_UttChoice_2019_05_29.csv"))
#klDivUttWorkers <- klDivUttWorkers[ , 2:ncol(klDivUttWorkers)]
paramsUttWorkers <- as.matrix(read.csv("X5_Data/KLDivUttParamsWorkers_x5_UttChoice_2019_05_29.csv"))
paramsUttWorkers <- paramsUttWorkers[ , 2:ncol(paramsUttWorkers)]

#klDivUttWorkers <- as.matrix(read.csv("X5_Data/KLDivUttWorkersWith.2_x5_2019_05_16.csv"))
#klDivUttWorkers <- klDivUttWorkers[ , 2:ncol(klDivUttWorkers)]
#paramsUttWorkers <- as.matrix(read.csv("X5_Data/KLDivUttParamsWorkersWith.2_x5_2019_05_16.csv"))
#paramsUttWorkers <- paramsUttWorkers[ , 2:ncol(paramsUttWorkers)]


#####################################################################################################
##############  TIME To determine and record the actual (optimized) Model Predictions ###############
#####################################################################################################
### 
# determining the model predictions after worker-specific model parameter optimization!
postListMat <- matrix(0,length(x5pilotData$X),9)
postListMat2 <- matrix(0,length(x5pilotData$X),9)
klDivValues <- matrix(NA,length(x5pilotData$X),3)
workerID <- -1
for(i in c(1:length(x5pilotData$X))) {
  objectConstellation <- c(obj1OC27[i],obj2OC27[i],obj3OC27[i])
  if(workerID != x5pilotData$workerid[i]) {
    workerID <- x5pilotData$workerid[i]
    params <- paramsUttWorkers[which(paramsUttWorkers[,1]==workerID)[1],c(4:4)]
   paramsAD <- paramsUttWorkers[which(paramsUttWorkers[,1]==workerID)[1],c(7:8)]
   paramsBD <- paramsUttWorkers[which(paramsUttWorkers[,1]==workerID)[1],c(5:6)]
   paramsAB <- paramsUttWorkers[which(paramsUttWorkers[,1]==workerID)[1],c(9:10)]
    paramsABD <- paramsUttWorkers[which(paramsUttWorkers[,1]==workerID)[1],c(11:13)]
    # print(params)
  }
  ##
  validUtterances <- determineValidUtterances(objectConstellation)
  ## determining the model predictions
#  postListMat[i,validUtterances] <- getBestInfGainUttPreferences(objectConstellation, 0, 0, 1, 1)
#  postListMat2[i,validUtterances] <- getBestInfGainUttPreferences(objectConstellation, 0, 0, 1, params[1])
#  postListMat[i,validUtterances] <- getBestInfGainUttPreferences(objectConstellation, paramsAD[1], 0, 1, paramsAD[2])
#  postListMat2[i,validUtterances] <- getBestInfGainUttPreferences(objectConstellation, 0, paramsBD[1], 1, paramsBD[2])
   postListMat[i,validUtterances] <- getBestInfGainUttPreferences(objectConstellation, paramsAB[1], paramsAB[2], 1, 1)
   postListMat2[i,validUtterances] <- getBestInfGainUttPreferences(objectConstellation, paramsABD[1], paramsABD[2], 1, paramsABD[3])
  ########### with respect to KLDivUttParamsWorkersWith.2_x5_2019_05_16.csv 
#  postListMat[i,validUtterances] <- getBestInfGainUttPreferences(objectConstellation, 0.2, 0.2, 1, 1)
#  postListMat2[i,validUtterances] <- getBestInfGainUttPreferences(objectConstellation, 0.2, 0.2, 1, params[1])
#  postListMat[i,validUtterances] <- getBestInfGainUttPreferences(objectConstellation, paramsAD[1], 0.2, 1, paramsAD[2])
#  postListMat2[i,validUtterances] <- getBestInfGainUttPreferences(objectConstellation, 0.2, paramsBD[1], 1, paramsBD[2])
  ## KL divergence values.... 
#  klDivValues[i,1] <- KLdivergence(bInfGainUttTurkers[i, validUtterances], rep(1/length(validUtterances), length(validUtterances)))
#  klDivValues[i,2] <- KLdivergence(bInfGainUttTurkers[i, validUtterances], postListMat[i, validUtterances])
#  klDivValues[i,3] <- KLdivergence(bInfGainUttTurkers[i, validUtterances], postListMat2[i, validUtterances])
}

###########
## adding all those values to the x4pilotData table.
subjectResponses <- round(bInfGainUttTurkers, digits=3)
colnames(subjectResponses) <- colnames(subjectResponses, do.NULL = FALSE, prefix = "DPost_")
x5pilotData <- data.frame(x5pilotData, as.data.frame(subjectResponses)) 

postListMat <- round(postListMat, digits=3)
colnames(postListMat) <- colnames(postListMat, do.NULL = FALSE, prefix = "MPost1_")
x5pilotData <- data.frame(x5pilotData, as.data.frame(postListMat)) 

postListMat2 <- round(postListMat2, digits=3)
colnames(postListMat2) <- colnames(postListMat2, do.NULL = FALSE, prefix = "MPost2_")
x5pilotData <- data.frame(x5pilotData, as.data.frame(postListMat2)) 

klDivValues <- round(klDivValues, digits=3)
colnames(klDivValues) <- colnames(klDivValues, do.NULL = FALSE, prefix = "KLDiv_")
x5pilotData <- data.frame(x5pilotData, as.data.frame(klDivValues)) 

#write.csv(x5pilotData, "X5_Data/x5pilotDataAugm_RSA_UttChoice_fixedAndD.csv")
#write.csv(x5pilotData, "X5_Data/x5pilotDataAugm_RSA_UttChoice_ADandBD.csv")
write.csv(x5pilotData, "X5_Data/x5pilotDataAugm_RSA_UttChoice_ABandABD.csv")






















