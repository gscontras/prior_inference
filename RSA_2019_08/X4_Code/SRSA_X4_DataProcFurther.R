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

#################################################
llWorkers12 <- as.matrix(read.csv("X4_Data/x4KLDivs_simpleRSA_indOpt_2019_1010.csv"))
paramsWorkers12 <- as.matrix(read.csv("X4_Data/x4Params_simpleRSA_indOpt_2019_1010.csv"))
llWorkers12 <- llWorkers12[,c(2:ncol(llWorkers12))]
paramsWorkers12 <- paramsWorkers12[,c(2:ncol(paramsWorkers12))]

############################################################################################
procType <- 3    ###########################################################################
############################################################################################

### 
# determining the model predictions after worker-specific model parameter optimization!
constellationCode <- matrix(0,length(x4pilotData$X),6)
uniqueCCode <- rep(0, length(x4pilotData$X))
postListMat1Opt <- matrix(0,length(x4pilotData$X),9)
postListMat2Opt <- matrix(0,length(x4pilotData$X),9)
postListMat1Opt <- matrix(0,length(x4pilotData$X),9)
postListMat2Opt <- matrix(0,length(x4pilotData$X),9)
logLik <- rep(0,length(x4pilotData$X))
workerID <- -1
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
    # all three optimized    params <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(11:13)]
    params1only <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(2)]
    params1only_obey.2 <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(3)]
    params12 <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(4:5)]
    # print(params)
  }
  ###############
  if(procType == 1) {
    postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice, 
                                                                  0, 0)
    postListMat2Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice, 
                                                                  .2, 0)
  }else if(procType == 2) {
    postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice, 
                                                                  abs(params1only[1]), 0)
    postListMat2Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice, 
                                                                  abs(params1only_obey.2[1]), .2)
  }else if(procType == 3) {
    postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice, 
                                                                  abs(params12[1]), abs(params12[2]))
    postListMat2Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice, 
                                                                  .2, .2)
  }
}

###########
## adding all those values to the x4pilotData table.
subjectResponsesOrdered <- round(subjectResponsesOrdered, digits=5)
colnames(subjectResponsesOrdered) <- colnames(subjectResponsesOrdered, do.NULL = FALSE, prefix = "DataPost_")
x4pilotData <- data.frame(x4pilotData, as.data.frame(subjectResponsesOrdered)) 

postListMat1Opt <- round(postListMat1Opt, digits=5)
colnames(postListMat1Opt) <- colnames(postListMat1Opt, do.NULL = FALSE, prefix = "Post1_")
consCodeAndPosteriors <- data.frame(as.data.frame(postListMat1Opt))
x4pilotData <- data.frame(x4pilotData, consCodeAndPosteriors) 

postListMat2Opt <- round(postListMat2Opt, digits=5)
colnames(postListMat2Opt) <- colnames(postListMat2Opt, do.NULL = FALSE, prefix = "Post2_")
consCodeAndPosteriorsNO <- data.frame(as.data.frame(postListMat2Opt))
x4pilotData <- data.frame(x4pilotData, consCodeAndPosteriorsNO) 

x4pilotData$CCode <- uniqueCCode
x4pilotData$logLik <- logLik

if(procType == 1) {
write.csv(x4pilotData, "X4_Data/x4pDataAugm_SRSAindOpt_fixed00_and_fixed.20.csv")
}else if(procType == 2) {
  write.csv(x4pilotData, "X4_Data/x4pDataAugm_SRSAindOpt_PrefStrengthOpt_obed0_and_obed.2.csv")
}else if(procType == 3) {
  write.csv(x4pilotData, "X4_Data/x4pDataAugm_SRSAindOpt_PrefandObedOpt_and_fixed.2.2.csv")
}
