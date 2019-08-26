source("CommonCode/SRSA_StratUtt.R")
source("CommonCode/SRSA_UttChoiceOptimization.R")

# TODO 
# ... this is taken from X6 and only partially adapted to X8
# ... still needs to be adapted such that it focuses on the relevant feature

# loading the raw pilot data (as Greg sent it on 2018/12/21)
x8pilotData <- read.csv("X8_Data/8-preference-type.csv")

## adding the 1-27 target and object2 & object3 code.
temp <- x8pilotData$obj1
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj1OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x8pilotData$obj1OC27 <- obj1OC27

temp <- x8pilotData$obj2
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj2OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x8pilotData$obj2OC27 <- obj2OC27

temp <- x8pilotData$obj3
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj3OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x8pilotData$obj3OC27 <- obj3OC27


################
## now get the preference slider values... 
sliderUtteranceTypeValues <- matrix(NA, nrow(x8pilotData), 6)
sliderSetValues <- matrix(NA,  nrow(x8pilotData), 6)

##########################################
# sets one column (index colIndex) of the sliderUttteranceTypes and sliderSet Values matrices.
# given the data vectors featureTypes and SliderValues 
# (i.e. x8pilotData$pref1/pref2../pref6 and x8pilotData$response1/response2/.../response6)
# @assumes that allUtterancesNew1 include the 9 possible feature type utterances
# @assumes the existence of the global matrices sliderUtteranceTypeValues and sliderSetValues
setSliderValues <- function(featureTypes, sliderValues, colIndex) {
  relRows <- which(!is.na(featureTypes) & !(""==featureTypes))
  for(j in c(1:length(relRows) ) ) { 
    sliderUtteranceTypeValues[relRows[j], colIndex] <<- which(allUtterancesNew1==featureTypes[relRows[j]])
    sliderSetValues[relRows[j], colIndex] <<- sliderValues[relRows[j]]
    #    print(c(which(allUtterancesNew1==featureTypes[relRows[j]]), j, relRows[j], colIndex))
  }
}

setSliderValues(x8pilotData$pref1, x8pilotData$response1, 1)
setSliderValues(x8pilotData$pref2, x8pilotData$response2, 2)
setSliderValues(x8pilotData$pref3, x8pilotData$response3, 3)
setSliderValues(x8pilotData$pref4, x8pilotData$response4, 4)
setSliderValues(x8pilotData$pref5, x8pilotData$response5, 5)
setSliderValues(x8pilotData$pref6, x8pilotData$response6, 6)

### normalizing the turker estimates and setting them into the corresponding matrix.
bInfGainUttTurkers <- matrix(NA, nrow(x8pilotData), 9)
for(i in c(1:nrow(x8pilotData)) ) {
  s <- sum(sliderSetValues[i,c(1:x8pilotData$numFeatures[i])])
  if(s > 0) {
    sliderSetValues[i,c(1:x8pilotData$numFeatures[i])] <- sliderSetValues[i,c(1:x8pilotData$numFeatures[i])] / s
  }else{ ## uniform distribution because all are set to 0
    sliderSetValues[i,c(1:x8pilotData$numFeatures[i])] <- 1 / (x8pilotData$numFeatures[i])
  }
  bInfGainUttTurkers[i, sliderUtteranceTypeValues[i,c(1:(x8pilotData$numFeatures[i]) )] ] <- 
    sliderSetValues[i,c(1:(x8pilotData$numFeatures[i]) )]
  for(j in c(1:x8pilotData$numFeatures[i])) {
    if(is.na(sliderSetValues[i,j])) {
      print("Error")
    }
  }
}

### encoding the relevant feature type still
targetFeatureTypes <- rep(0, nrow(x8pilotData))
for(i in c(1:nrow(x8pilotData)) ) {
  targetFeatureTypes[i] <- which(allFeatureTypesNew1 == x8pilotData$targetFeature[i])
}


#######################################################
## reloading optimization values
#######################################################
paramsUttWorkers <- as.matrix(read.csv("X8_Data/KLDivUttParamsWorkers_x8_UttChoiceSimpleRSAFTF_2019_06_17.csv"))
paramsUttWorkers <- paramsUttWorkers[ , 2:ncol(paramsUttWorkers)]
#paramsUttWorkers <- as.matrix(read.csv("X8_Data/KLDivUttParamsWorkersWith.2_x8_UttChoiceSimpleRSAFTF_2019_06_17.csv"))
#paramsUttWorkers <- paramsUttWorkers[ , 2:ncol(paramsUttWorkers)]

#####################################################################################################
##############  TIME To determine and record the actual (optimized) Model Predictions ###############
#####################################################################################################
### 
# determining the model predictions after worker-specific model parameter optimization!
postListMat <- matrix(0,length(x8pilotData$X),9)
postListMat2 <- matrix(0,length(x8pilotData$X),9)
klDivValues <- matrix(NA,length(x8pilotData$X),3)
workerID <- -1
for(i in c(1:length(x8pilotData$X))) {
  objectConstellation <- c(obj1OC27[i],obj2OC27[i],obj3OC27[i])
  if(workerID != x8pilotData$workerid[i]) {
    workerID <- x8pilotData$workerid[i]
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
  postListMat[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, 0, 0, 1)
  postListMat2[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, 0, 0, params[1])
#  postListMat[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, paramsAD[1], 0, paramsAD[2])
#  postListMat2[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, 0, paramsBD[1], paramsBD[2])
#  postListMat[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, paramsAB[1], paramsAB[2], 1)
# postListMat2[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, paramsABD[1], paramsABD[2], paramsABD[3])
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
x8pilotData <- data.frame(x8pilotData, as.data.frame(subjectResponses)) 

postListMat <- round(postListMat, digits=3)
colnames(postListMat) <- colnames(postListMat, do.NULL = FALSE, prefix = "MPost1_")
x8pilotData <- data.frame(x8pilotData, as.data.frame(postListMat)) 

postListMat2 <- round(postListMat2, digits=3)
colnames(postListMat2) <- colnames(postListMat2, do.NULL = FALSE, prefix = "MPost2_")
x8pilotData <- data.frame(x8pilotData, as.data.frame(postListMat2)) 

klDivValues <- round(klDivValues, digits=3)
colnames(klDivValues) <- colnames(klDivValues, do.NULL = FALSE, prefix = "KLDiv_")
x8pilotData <- data.frame(x8pilotData, as.data.frame(klDivValues)) 

write.csv(x8pilotData, "X8_Data/x8pilotDataAugm_UttChoice_SRSA_fixedAndD.csv")
#write.csv(x8pilotData, "X8_Data/x8pilotDataAugm_UttChoice_SRSA_ADandBD.csv")
#write.csv(x8pilotData, "X8_Data/x8pilotDataAugm_UttChoice_SRSA_ABandABD.csv")




