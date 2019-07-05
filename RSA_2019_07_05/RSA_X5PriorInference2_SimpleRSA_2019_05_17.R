source("RSA_StratUttOptimization_SimpleRSA_2019_0507.R")
source("RSA_StratUttModelSimple_2019_0505.R")

# loading the raw pilot data (as Greg sent it on 2019/01/16)
x5priorInfData <- read.csv("5-combined-unique.csv")
x5priorInfData <- x5priorInfData[which(x5priorInfData$trial_type=="prior_inference"),]

# adding feature property codes (which feature was uttereed, which features were questioned)
uttFeat <- ifelse(x5priorInfData$utterance=="green" | x5priorInfData$utterance=="red" | x5priorInfData$utterance=="blue", 3,
                  ifelse(x5priorInfData$utterance=="solid" | x5priorInfData$utterance=="striped" | x5priorInfData$utterance=="polka-dotted", 2, 1))
x5priorInfData$uttFeat <- uttFeat

q1Feat <- ifelse(x5priorInfData$pref1=="green things" | x5priorInfData$pref1=="red things" | x5priorInfData$pref1=="blue things", 3,
                 ifelse(x5priorInfData$pref1=="solid things" | x5priorInfData$pref1=="striped things" | x5priorInfData$pref1=="polka-dotted things", 2, 
                        ifelse(x5priorInfData$pref1=="clouds" | x5priorInfData$pref1=="circles" | x5priorInfData$pref1=="squares", 1,
                               -1 ) ))
x5priorInfData$q1Feat <- q1Feat

q2Feat <- ifelse(x5priorInfData$pref4=="green things" | x5priorInfData$pref4=="red things" | x5priorInfData$pref4=="blue things", 3,
                 ifelse(x5priorInfData$pref4=="solid things" | x5priorInfData$pref4=="striped things" | x5priorInfData$pref4=="polka-dotted things", 2, 
                        ifelse(x5priorInfData$pref4=="clouds" | x5priorInfData$pref4=="circles" | x5priorInfData$pref4=="squares", 1,
                               -1 ) ))
x5priorInfData$q2Feat <- q2Feat

## adding the 1-27 target and object2 & object3 code.
temp <- x5priorInfData$target
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
targetOC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x5priorInfData$targetOC27 <- targetOC27

temp <- x5priorInfData$obj2
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj2OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x5priorInfData$obj2OC27 <- obj2OC27

temp <- x5priorInfData$obj3
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj3OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x5priorInfData$obj3OC27 <- obj3OC27

## now determining the recorded subject responses 
subjectResponses <- matrix(0,length(x5priorInfData$X),6)
# postListMat <- matrix(0,length(pilotData$X),9)
# logLik <- rep(0,length(pilotData$X))
for(i in c(1:length(x5priorInfData$X))) {
  # objectConstellation <- c(targetOC27[i],obj2OC27[i],obj3OC27[i])
  # featChoice <- uttFeat[i]
  # postListMat[i,] <- determineSpeakerPostListPrefs(objectConstellation, featChoice, .1, 0)
  # print(c(objectConstellation, featChoice))
  #
  subjectResponses[i,1] <- x5priorInfData$response1[i] + 1e-100
  subjectResponses[i,2] <- x5priorInfData$response2[i] + 1e-100
  subjectResponses[i,3] <- x5priorInfData$response3[i] + 1e-100
  subjectResponses[i,4] <- x5priorInfData$response4[i] + 1e-100
  subjectResponses[i,5] <- x5priorInfData$response5[i] + 1e-100
  subjectResponses[i,6] <- x5priorInfData$response6[i] + 1e-100
  subjectResponses[i,1:3] <- subjectResponses[i,1:3] / sum(subjectResponses[i,1:3])
  subjectResponses[i,4:6] <- subjectResponses[i,4:6] / sum(subjectResponses[i,4:6])
}

## ordering the recorded subject responses such that they can be compared directly 
#   compared to the model predictions 
##             (particularly for visual comparison in the table) 
subjectResponsesOrdered <- matrix(NA ,length(x5priorInfData$X),9)
for(i in c(1:length(x5priorInfData$X))) {
  for(j in 1:3) {  
    subjectResponsesOrdered[i, (j+(q1Feat[i]-1)*3)] <- subjectResponses[i,j]
  }
  for(j in 1:3) {  
    subjectResponsesOrdered[i, (j+(q2Feat[i]-1)*3)] <- subjectResponses[i,3+j]
  }
}
subjectResponsesOrdered <- round(subjectResponsesOrdered, digits=5)

##########################################################
procType <- 3 ### 1 or 2 or 3!
##########################################################

## recording KL divergence and parameters (base model, 1 param, 2 params)
workerIDs <- x5priorInfData$workerid
idMax <- max(workerIDs)

## reading in optimized kl div and value tables.
paramsWorkers12 <- as.matrix(read.csv("x5PriorInfModelsOptParamsSimpleRSA_2019_05_17.csv"))
paramsWorkers12 <- paramsWorkers12[,c(2:ncol(paramsWorkers12))]

### 
# determining the model predictions after worker-specific model parameter optimization!
constellationCode <- matrix(0,length(x5priorInfData$X),6)
uniqueCCode <- rep(0, length(x5priorInfData$X))
postListMat1Opt <- matrix(0,length(x5priorInfData$X),9)
postListMat2Opt <- matrix(0,length(x5priorInfData$X),9)
logLik <- rep(0,length(x5priorInfData$X))
workerID <- -1
for(i in c(1:length(x5priorInfData$X))) {
  objectConstellation <- c(targetOC27[i],obj2OC27[i],obj3OC27[i])
  featChoice <- uttFeat[i]
  constellationCode[i,] <- getConstellationCode(objectConstellation, featChoice)[[1]]
  uc <- 0
  for(j in c(1:6)) {
    uc <- (uc * 10) + constellationCode[i,j]
  }
  uniqueCCode[i] <- uc
  if(workerID != x5priorInfData$workerid[i]) {
    workerID <- x5priorInfData$workerid[i]
    # all three optimized    params <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(11:13)]
    params1 <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(2)]
    params1no.2 <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(3)]
    params2 <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(4)]
    params1no.1 <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(5)]
    params2p.2 <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(6)]
    
    params12 <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(7,8)]
  }
  if(procType == 1) {
    postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice,
                                                                  abs(params1[1]), 0)
    postListMat2Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice,
                                                                  0, abs(params2[1]))
  }else if(procType == 2) {
    postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice,
                                                                  abs(params1no.1[1]), .1)
    postListMat2Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice,
                                                                  abs(params1no.2[1]), .2)
  }else if(procType == 3) {
    postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice,
                                                                  .2, abs(params2p.2[1]))
    postListMat2Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice,
                                                                  abs(params12[1]), abs(params12[2]))
    
  }
}


###########
## adding all those values to the x5priorInfData table.
subjectResponsesOrdered <- round(subjectResponsesOrdered, digits=5)
colnames(subjectResponsesOrdered) <- colnames(subjectResponsesOrdered, do.NULL = FALSE, prefix = "DataPost_")
x5priorInfData <- data.frame(x5priorInfData, as.data.frame(subjectResponsesOrdered)) 

postListMat1Opt <- round(postListMat1Opt, digits=5)
colnames(postListMat1Opt) <- colnames(postListMat1Opt, do.NULL = FALSE, prefix = "Post1_")
consCodeAndPosteriors <- data.frame(as.data.frame(postListMat1Opt))
x5priorInfData <- data.frame(x5priorInfData, consCodeAndPosteriors) 

postListMat2Opt <- round(postListMat2Opt, digits=5)
colnames(postListMat2Opt) <- colnames(postListMat2Opt, do.NULL = FALSE, prefix = "Post2_")
consCodeAndPosteriorsNO <- data.frame(as.data.frame(postListMat2Opt))
x5priorInfData <- data.frame(x5priorInfData, consCodeAndPosteriorsNO) 

x5priorInfData$CCode <- uniqueCCode
x5priorInfData$logLik <- logLik

if(procType == 1) {
  write.csv(x5priorInfData, "x5priorInfDataAugm_SimpleRSA_P1or2_2019_05_17.csv")
}else if(procType == 2) {
  write.csv(x5priorInfData, "x5priorInfDataAugm_SimpleRSA_P1no.1or.2_2019_05_17.csv")
}else if(procType == 3) {
  write.csv(x5priorInfData, "x5priorInfDataAugm_SimpleRSA_P2pref.2andP12_2019_05_17.csv")
}



