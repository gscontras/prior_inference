source("CommonCode/SRSA_StratUtt.R")
source("CommonCode/SRSA_StratUttOptimization.R")

# loading the raw pilot data (as Greg sent it on 2018/12/21)
x4pilotData <- read.csv("X4_Data/4-pilot-training.csv")

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

## now determining the recorded subject responses 
subjectResponses <- matrix(0,length(x4pilotData$X),6)
# postListMat <- matrix(0,length(pilotData$X),9)
# logLik <- rep(0,length(pilotData$X))
for(i in c(1:length(x4pilotData$X))) {
  # objectConstellation <- c(targetOC27[i],obj2OC27[i],obj3OC27[i])
  # featChoice <- uttFeat[i]
  # postListMat[i,] <- determineSpeakerPostListPrefs(objectConstellation, featChoice, .1, 0)
  # print(c(objectConstellation, featChoice))
  #
  subjectResponses[i,1] <- x4pilotData$response1[i] + 1e-100
  subjectResponses[i,2] <- x4pilotData$response2[i] + 1e-100
  subjectResponses[i,3] <- x4pilotData$response3[i] + 1e-100
  subjectResponses[i,4] <- x4pilotData$response4[i] + 1e-100
  subjectResponses[i,5] <- x4pilotData$response5[i] + 1e-100
  subjectResponses[i,6] <- x4pilotData$response6[i] + 1e-100
  subjectResponses[i,1:3] <- subjectResponses[i,1:3] / sum(subjectResponses[i,1:3])
  subjectResponses[i,4:6] <- subjectResponses[i,4:6] / sum(subjectResponses[i,4:6])
}

## ordering the recorded subject responses such that they can be compared directly 
#   compared to the model predictions 
##             (particularly for visual comparison in the table) 
subjectResponsesOrdered <- matrix(NA ,length(x4pilotData$X),9)
for(i in c(1:length(x4pilotData$X))) {
  for(j in 1:3) {  
    subjectResponsesOrdered[i, (j+(q1Feat[i]-1)*3)] <- subjectResponses[i,j]
  }
  for(j in 1:3) {  
    subjectResponsesOrdered[i, (j+(q2Feat[i]-1)*3)] <- subjectResponses[i,3+j]
  }
}
subjectResponsesOrdered <- round(subjectResponsesOrdered, digits=5)


## recording KL divergence and parameters (base model, 1 param, 2 params)
workerIDs <- x4pilotData$workerid
idMax <- max(workerIDs)

##############################################################################
##############################################################################

## loading optimized trial-respective cross-validated parameter values 
#paramsWorkers1 <- as.matrix(read.csv("X4_Data/x4CrossVal_SimpleRSA_Params_1parOpt_2019_0430.csv"))
#paramsWorkers1 <- paramsWorkers1[,c(2:ncol(paramsWorkers1))]
#paramsWorkers1notObey.1 <- as.matrix(read.csv("X4_Data/x4CrossVal_SimpleRSA_Params_notObey.1_2019_0430.csv"))
#paramsWorkers1notObey.1 <- paramsWorkers1notObey.1[,c(2:ncol(paramsWorkers1notObey.1))]

paramsWorkers1 <- as.matrix(read.csv("X4_Data/x4Params_fullRSA_crossVal_1parOpt_2019_1008.csv"))
paramsWorkers1 <- paramsWorkers1[,c(2:ncol(paramsWorkers1))]
paramsWorkers1notObey.1 <- as.matrix(read.csv("X4_Data/x4Params_simpleRSA_crossVal_1parOptnotObej.1_2019_1009.csv"))
paramsWorkers1notObey.1 <- paramsWorkers1notObey.1[,c(2:ncol(paramsWorkers1notObey.1))]
paramsWorkers2 <- as.matrix(read.csv("X4_Data/x4Params_simpleRSA_crossVal_2parOpt_2019_1009.csv"))
paramsWorkers2 <- paramsWorkers2[,c(2:ncol(paramsWorkers2))]


############################################################################################
procType <- 2   ###########################################################################
############################################################################################

### 
# determining the model predictions after worker-specific model parameter optimization!
constellationCode <- matrix(0,length(x4pilotData$X),6)
uniqueCCode <- rep(0, length(x4pilotData$X))
postListMat1 <- matrix(0,length(x4pilotData$X),9)
postListMat2 <- matrix(0,length(x4pilotData$X),9)
logLik <- rep(0,length(x4pilotData$X))
workerID <- -1
workerIndex <- 0 ### starting with zero and -1 worker ID...
### as a result, workerIndex is increase by one  in the first for-loop iteration
trialIndex <- 1
for(i in c(1:length(x4pilotData$X))) {
  objectConstellation <- c(targetOC27[i],obj2OC27[i],obj3OC27[i])
  featChoice <- uttFeat[i]
  constellationCode[i,] <- getConstellationCode(objectConstellation, featChoice)[[1]]
  uc <- 0
  for(j in c(1:6)) {
    uc <- (uc * 10) + constellationCode[i,j]
  }
  uniqueCCode[i] <- uc
  if(workerID != x4pilotData$workerid[i]) {
    workerID <- x4pilotData$workerid[i]
    workerIndex <- workerIndex + 1
    trialIndex <- 1
  }
  if(procType == 1) {
    params1 <- paramsWorkers1[workerIndex, trialIndex]
    postListMat1[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice,
                                                               params1[1], 0) 
    params2 <- paramsWorkers2[workerIndex, (((trialIndex-1)*2+1):((trialIndex-1)*2+2))]
    postListMat2[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice,
                                                               params2[1], params2[2]) 
  } else if(procType == 2) {
    params1nO.1 <- paramsWorkers1notObey.1[workerIndex, trialIndex]
    postListMat1[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice,
                                                               params1nO.1[1], 0.1) 
    postListMat2[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice,
                                                               0.1, 0.1) 
    
  } 
  trialIndex <- trialIndex + 1
}
# now determine expected log likelihoods given the subject responses and the optimized model values.
logLik <- rep(0,length(x4pilotData$X))
for(i in c(1:length(x4pilotData$X))) {
  for(j in 1:3) {  
    logLik[i] <- logLik[i] - subjectResponses[i,j] * 
      log(postListMat1[i, j+(q1Feat[i]-1)*3])
  }
  for(j in 1:3) {  
    logLik[i] <- logLik[i] - subjectResponses[i,3+j] * 
      log(postListMat1[i, j+(q2Feat[i]-1)*3])
  }
}

###########
## adding all those values to the x4pilotData table.
subjectResponsesOrdered <- round(subjectResponsesOrdered, digits=5)
colnames(subjectResponsesOrdered) <- colnames(subjectResponsesOrdered, do.NULL = FALSE, prefix = "DataPost_")
x4pilotData <- data.frame(x4pilotData, as.data.frame(subjectResponsesOrdered)) 

postListMat1 <- round(postListMat1, digits=5)
colnames(postListMat1) <- colnames(postListMat1, do.NULL = FALSE, prefix = "Post1_")
consCodeAndPosteriors <- data.frame(as.data.frame(postListMat1))
x4pilotData <- data.frame(x4pilotData, consCodeAndPosteriors) 

postListMat2 <- round(postListMat2, digits=5)
colnames(postListMat2) <- colnames(postListMat2, do.NULL = FALSE, prefix = "Post2_")
consCodeAndPosteriorsNO <- data.frame(as.data.frame(postListMat2))
x4pilotData <- data.frame(x4pilotData, consCodeAndPosteriorsNO) 

x4pilotData$CCode <- uniqueCCode
x4pilotData$logLik <- logLik

if(procType == 1) {
  write.csv(x4pilotData, "X4_Data/x4pDataAugm_SRSAcrossVal_Opt1_and_Opt2.csv")
} else if(procType == 2) {
  write.csv(x4pilotData, "X4_Data/x4pDataAugm_SRSAcrossVal_Opt1obed.1_and_fixed.1.1.csv")
} 


