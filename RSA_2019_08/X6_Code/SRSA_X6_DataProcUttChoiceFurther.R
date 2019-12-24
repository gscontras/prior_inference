source("CommonCode/SRSA_StratUtt.R")
source("CommonCode/SRSA_UttChoiceOptimization.R")

# loading the raw pilot data (as Greg sent it on 2018/12/21)
x6pilotData <- read.csv("X6_Data/6-combined-revised.csv")
x6pilotData <- x6pilotData[which(x6pilotData$trial_type=="utterance_choice"),]

## adding the 1-27 target and object2 & object3 code.
temp <- x6pilotData$obj1
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj1OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x6pilotData$obj1OC27 <- obj1OC27

temp <- x6pilotData$obj2
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj2OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x6pilotData$obj2OC27 <- obj2OC27

temp <- x6pilotData$obj3
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj3OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x6pilotData$obj3OC27 <- obj3OC27

## now identify the first column number of the turker sliders and response pairs
sliderIndex <- grep("^pref1", colnames(x6pilotData))
## and use that index to determine all slider identities and corresponding slider values.
sliderUtteranceTypes <- matrix(NA, nrow(x6pilotData), 9)
sliderSetValues <- matrix(NA,  nrow(x6pilotData), 9)
for(i in c(1:9)) {
  colIndex <- sliderIndex + (i-1) * 2
  relRows <- which(!is.na(x6pilotData[[colIndex]]))
  for(j in c(1:length(relRows) ) ) { 
    sliderUtteranceTypes[relRows[j], i] <- which(allUtterancesNew1==x6pilotData[[colIndex]][relRows[j]])
    sliderSetValues[relRows[j], i] <- x6pilotData[[colIndex+1]][relRows[j]]
  }
}
### normalizing the turker estimates and setting them into the corresponding matrix.
bInfGainUttTurkers <- matrix(NA, nrow(x6pilotData), 9)
for(i in c(1:nrow(x6pilotData)) ) {
  s <- sum(sliderSetValues[i,c(1:x6pilotData$numFeatures[i])])
  if(s > 0) {
    sliderSetValues[i,c(1:x6pilotData$numFeatures[i])] <- sliderSetValues[i,c(1:x6pilotData$numFeatures[i])] / s
  }else{
    sliderSetValues[i,c(1:x6pilotData$numFeatures[i])] <- 1 / (x6pilotData$numFeatures[i])
  }
  bInfGainUttTurkers[i, sliderUtteranceTypes[i,c(1:(x6pilotData$numFeatures[i]) )] ] <- sliderSetValues[i,c(1:(x6pilotData$numFeatures[i]) )]
  for(j in c(1:x6pilotData$numFeatures[i])) {
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
klDivUttWorkers <- as.matrix(read.csv("X6_Data/KLDivUttWorkers_x6_UttChoiceSimpleRSA_2019_06_03.csv"))
klDivUttWorkers <- klDivUttWorkers[ , 2:ncol(klDivUttWorkers)]
paramsUttWorkers <- as.matrix(read.csv("X6_Data/KLDivUttParamsWorkers_x6_UttChoiceSimpleRSA_2019_06_03.csv"))
paramsUttWorkers <- paramsUttWorkers[ , 2:ncol(paramsUttWorkers)]

#klDivUttWorkers <- as.matrix(read.csv("X6_Data/KLDivUttWorkersWith.2_x6_2019_05_16.csv"))
#klDivUttWorkers <- klDivUttWorkers[ , 2:ncol(klDivUttWorkers)]
#paramsUttWorkers <- as.matrix(read.csv("X6_Data/KLDivUttParamsWorkersWith.2_x6_2019_05_16.csv"))
#paramsUttWorkers <- paramsUttWorkers[ , 2:ncol(paramsUttWorkers)]


#####################################################################################################
##############  TIME To determine and record the actual (optimized) Model Predictions ###############
#####################################################################################################
### 
# determining the model predictions after worker-specific model parameter optimization!
postListMat <- matrix(0,length(x6pilotData$X),9)
postListMat2 <- matrix(0,length(x6pilotData$X),9)
klDivValues <- matrix(NA,length(x6pilotData$X),3)
workerID <- -1
for(i in c(1:length(x6pilotData$X))) {
  objectConstellation <- c(obj1OC27[i],obj2OC27[i],obj3OC27[i])
  if(workerID != x6pilotData$workerid[i]) {
    workerID <- x6pilotData$workerid[i]
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
  postListMat[i,validUtterances] <- rep((1/length(validUtterances)),length(validUtterances))
  postListMat2[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, 0, 0, 0)
  #  postListMat[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, 0, 0, 1)
#  postListMat2[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, 0, 0, params[1])
#  postListMat[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, paramsAD[1], 0, paramsAD[2])
#  postListMat2[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, 0, paramsBD[1], paramsBD[2])
#   postListMat[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, paramsAB[1], paramsAB[2], 1)
 #  postListMat2[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, paramsABD[1], paramsABD[2], paramsABD[3])
  ########### with respect to KLDivUttParamsWorkersWith.2_x6_2019_05_16.csv 
#  postListMat[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, 0.2, 0.2, 1)
#  postListMat2[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, 0.2, 0.2, params[1])
#  postListMat[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, paramsAD[1], 0.2, paramsAD[2])
#  postListMat2[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, 0.2, paramsBD[1], paramsBD[2])
  ## KL divergence values.... 
#  klDivValues[i,1] <- KLdivergence(bInfGainUttTurkers[i, validUtterances], rep(1/length(validUtterances), length(validUtterances)))
#  klDivValues[i,2] <- KLdivergence(bInfGainUttTurkers[i, validUtterances], postListMat[i, validUtterances])
#  klDivValues[i,3] <- KLdivergence(bInfGainUttTurkers[i, validUtterances], postListMat2[i, validUtterances])
}

###########
## adding all those values to the x4pilotData table.
subjectResponses <- round(bInfGainUttTurkers, digits=3)
colnames(subjectResponses) <- colnames(subjectResponses, do.NULL = FALSE, prefix = "DPost_")
x6pilotData <- data.frame(x6pilotData, as.data.frame(subjectResponses)) 

postListMat <- round(postListMat, digits=3)
colnames(postListMat) <- colnames(postListMat, do.NULL = FALSE, prefix = "MPost1_")
x6pilotData <- data.frame(x6pilotData, as.data.frame(postListMat)) 

postListMat2 <- round(postListMat2, digits=3)
colnames(postListMat2) <- colnames(postListMat2, do.NULL = FALSE, prefix = "MPost2_")
x6pilotData <- data.frame(x6pilotData, as.data.frame(postListMat2)) 

klDivValues <- round(klDivValues, digits=3)
colnames(klDivValues) <- colnames(klDivValues, do.NULL = FALSE, prefix = "KLDiv_")
x6pilotData <- data.frame(x6pilotData, as.data.frame(klDivValues)) 

write.csv(x6pilotData, "X6_Data/x6pilotDataAugm_UttChoice_SRSA_BaseAndFixed000_2019_12_24.csv")
#write.csv(x6pilotData, "X6_Data/x6pilotDataAugm_UttChoice_SRSA_fixedAndD_2019_05_29.csv")
#write.csv(x6pilotData, "X6_Data/x6pilotDataAugm_UttChoice_SRSA_ADandBD_052019.csv")
#write.csv(x6pilotData, "X6_Data/x6pilotDataAugm_UttChoice_SRSA_ABandABD_052019.csv")






















