source("CommonCode/SRSA_StratUtt.R")
source("CommonCode/SRSA_StratUttOptimization.R")

# loading the raw pilot data (as Greg sent it on 2018/11)
x2pilotData<- read.csv("X2_Data/2-pilot.csv")

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

## now determining the recorded subject responses 
subjectResponses <- matrix(0,length(x2pilotData$X),6)
# postListMat <- matrix(0,length(pilotData$X),9)
# logLik <- rep(0,length(pilotData$X))
for(i in c(1:length(x2pilotData$X))) {
  # objectConstellation <- c(targetOC27[i],obj2OC27[i],obj3OC27[i])
  # featChoice <- uttFeat[i]
  # postListMat[i,] <- determineSpeakerPostListPrefs(objectConstellation, featChoice, .1, 0)
  # print(c(objectConstellation, featChoice))
  #
  subjectResponses[i,1] <- x2pilotData$response1[i] + 1e-100
  subjectResponses[i,2] <- x2pilotData$response2[i] + 1e-100
  subjectResponses[i,3] <- x2pilotData$response3[i] + 1e-100
  subjectResponses[i,4] <- x2pilotData$response4[i] + 1e-100
  subjectResponses[i,5] <- x2pilotData$response5[i] + 1e-100
  subjectResponses[i,6] <- x2pilotData$response6[i] + 1e-100
  subjectResponses[i,1:3] <- subjectResponses[i,1:3] / sum(subjectResponses[i,1:3])
  subjectResponses[i,4:6] <- subjectResponses[i,4:6] / sum(subjectResponses[i,4:6])
}

## ordering the recorded subject responses such that they can be compared directly 
#   compared to the model predictions 
##             (particularly for visual comparison in the table) 
subjectResponsesOrdered <- matrix(NA ,length(x2pilotData$X),9)
for(i in c(1:length(x2pilotData$X))) {
  for(j in 1:3) {  
    subjectResponsesOrdered[i, (j+(q1Feat[i]-1)*3)] <- subjectResponses[i,j]
  }
  for(j in 1:3) {  
    subjectResponsesOrdered[i, (j+(q2Feat[i]-1)*3)] <- subjectResponses[i,3+j]
  }
}
subjectResponsesOrdered <- round(subjectResponsesOrdered, digits=5)


## recording KL divergence and parameters (base model, 1 param, 2 params)
workerIDs <- x2pilotData$workerid
idMax <- max(workerIDs)

##############################################################################
##############################################################################

processType <- 2 # 1=opt.pref.,obey/notObey.1; 2=2 or fixed

## loading optimized trial-respective cross-validated parameter values 
if(processType==1) {
  paramsWorkers1 <- as.matrix(read.csv("X2_Data/x2CrossVal_SimpleRSA_Params_1parOpt_2019_0430.csv")) 
  paramsWorkers1notObey.1 <- as.matrix(read.csv("X2_Data/x2CrossVal_SimpleRSA_Params_notObey.1_2019_0430.csv"))
  paramsWorkers1 <- paramsWorkers1[,c(2:ncol(paramsWorkers1))]
  paramsWorkers1notObey.1 <- paramsWorkers1notObey.1[,c(2:ncol(paramsWorkers1notObey.1))]
}else if(processType==2) {
  paramsWorkers12 <- as.matrix(read.csv("X2_Data/x2CrossVal_SimpleRSA_Params_12parOpt_2019_0430.csv"))
#  paramsWorkers123 <- as.matrix(read.csv("X2_Data/x2CrossVal_Params_123parOpt_2019_0430.csv"))
  paramsWorkers12 <- paramsWorkers12[,c(2:ncol(paramsWorkers12))]
#  paramsWorkers123 <- paramsWorkers123[,c(2:ncol(paramsWorkers123))]
}
### 
# determining the model predictions after worker-specific model parameter optimization!
constellationCode <- matrix(0,length(x2pilotData$X),6)
uniqueCCode <- rep(0, length(x2pilotData$X))
postListMat1 <- matrix(0,length(x2pilotData$X),9)
postListMat2 <- matrix(0,length(x2pilotData$X),9)
logLik <- rep(0,length(x2pilotData$X))
workerID <- -1
workerIndex <- 0 ### starting with zero and -1 worker ID...
### as a result, workerIndex is increase by one  in the first for-loop iteration
trialIndex <- 1
for(i in c(1:length(x2pilotData$X))) {
  objectConstellation <- c(targetOC27[i],obj2OC27[i],obj3OC27[i])
  featChoice <- uttFeat[i]
  constellationCode[i,] <- getConstellationCode(objectConstellation, featChoice)[[1]]
  uc <- 0
  for(j in c(1:6)) {
    uc <- (uc * 10) + constellationCode[i,j]
  }
  uniqueCCode[i] <- uc
  if(workerID != x2pilotData$workerid[i]) {
    workerID <- x2pilotData$workerid[i]
    workerIndex <- workerIndex + 1
    trialIndex <- 1
  }
  
  if(processType==1) {
    params1 <- paramsWorkers1[workerIndex, trialIndex]
    postListMat1[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice, 
                                                      params1[1], 0)
    params1no <- paramsWorkers1notObey.1[workerIndex, trialIndex]
    postListMat2[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice, 
                                                      params1no[1], 0.1)
  }else if(processType==2) {
    params12 <- paramsWorkers12[workerIndex, (((trialIndex-1)*2+1):((trialIndex-1)*2+2))]
    postListMat1[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice, 
                                                        abs(params12[1]), abs(params12[2]))
    postListMat2[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice, 
                                                     0, 0.1)
  }
  trialIndex <- trialIndex + 1
}
# now determine expected log likelihoods given the subject responses and the optimized model values.
logLik <- rep(0,length(x2pilotData$X))
for(i in c(1:length(x2pilotData$X))) {
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
## adding all those values to the x2pilotData table.
subjectResponsesOrdered <- round(subjectResponsesOrdered, digits=5)
colnames(subjectResponsesOrdered) <- colnames(subjectResponsesOrdered, do.NULL = FALSE, prefix = "DataPost_")
x2pilotData <- data.frame(x2pilotData, as.data.frame(subjectResponsesOrdered)) 

postListMat1 <- round(postListMat1, digits=5)
colnames(postListMat1) <- colnames(postListMat1, do.NULL = FALSE, prefix = "Post1_")
consCodeAndPosteriors <- data.frame(as.data.frame(postListMat1))
x2pilotData <- data.frame(x2pilotData, consCodeAndPosteriors) 

postListMat2 <- round(postListMat2, digits=5)
colnames(postListMat2) <- colnames(postListMat2, do.NULL = FALSE, prefix = "Post2_")
consCodeAndPosteriorsNO <- data.frame(as.data.frame(postListMat2))
x2pilotData <- data.frame(x2pilotData, consCodeAndPosteriorsNO) 

x2pilotData$CCode <- uniqueCCode
x2pilotData$logLik <- logLik


if(processType==1) {
  write.csv(x2pilotData, "X2_Data/x2pilot_DataCrossValAugmented1notObey.1_2019_0513.csv")
}else if(processType==2) {
  write.csv(x2pilotData, "X2_Data/x2pilot_DataCrossValAugmented2orFixed00.1_2019_0513.csv")
}


