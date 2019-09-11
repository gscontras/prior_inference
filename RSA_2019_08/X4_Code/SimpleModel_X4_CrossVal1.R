source("CommonCode/RSA_StratUtt.R")
source("CommonCode/RSA_StratUttOptimization.R")
source("CommonCode/SimpleModel_StratUtt.R")

# loading the raw pilot data (as Greg sent it on 2018/12/21)
x4pilotData <- read.csv("X4_Data/4-pilot-training.csv")

############# Analyzing Object Choices with Simple Preference Model ########################

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
#   to the model predictions 
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
uniqueItemCodes <- sort(unique(x4pilotData$itemCode))

#######################
## Optimizing wrt KL divergence criterion
# parameter-based SPM optimizations...
# leave one out parameter determination for each participant!
numParams <- 2

klDivWorkers <- matrix(0,length(unique(workerIDs)), 15)
klDivCrossValValues <- matrix(0,length(unique(workerIDs)), 15)
paramsWorkers <- matrix(0,length(unique(workerIDs)), 15*numParams)

workerIndex <- 1
for(workerID in c(0:idMax)) {
  idICases <- which(workerIDs == workerID)
  if(length(idICases)>0) {
    print(c("Optimizing cross. val. w.r.t. worker with ID: ",workerID," = index: ",workerIndex))
    workerItemCodes <- (x4pilotData$itemCode)[idICases]
    ## now moving through the trials and optimizing with respect to the other trials
    for(workerTrialIndex in c(1:length(idICases))) {
      trialItemCode <- workerItemCodes[workerTrialIndex]
      filteredTrialIndices <- which(workerItemCodes != trialItemCode)
      consideredIdICases <- idICases[filteredTrialIndices]
      ## generating data matrix containing the considered cases
      dataWorker <- matrix(0, length(consideredIdICases), 12)
      dataWorker[,1] <- targetOC27[consideredIdICases]
      dataWorker[,2] <- obj2OC27[consideredIdICases]
      dataWorker[,3] <- obj3OC27[consideredIdICases]
      dataWorker[,4] <- uttFeat[consideredIdICases]
      dataWorker[,5] <- q1Feat[consideredIdICases]
      dataWorker[,6] <- q2Feat[consideredIdICases]
      dataWorker[,7:12] <- subjectResponses[consideredIdICases,1:6]
      ##
      if(numParams==1) {
        optRes1 <- optimize(choiceBasedModelKLDiv1param, c(0,1), dataWorker)
        klDivWorkers[workerIndex,workerTrialIndex] <- optRes1$objective
        paramsWorkers[workerIndex, workerTrialIndex] <- optRes1$minimum
      }else{
        optRes2 <- optim(c(.5, 1/3), choiceBasedModelKLDiv2params, method="L-BFGS-B", gr=NULL, dataWorker,
                           lower = c(0,0), upper = c(1,1))
        klDivWorkers[workerIndex,workerTrialIndex] <- optRes2$value
        paramsWorkers[workerIndex,(workerTrialIndex-1)*2+1] <- optRes2$par[1]
        paramsWorkers[workerIndex,(workerTrialIndex-1)*2+2] <- optRes2$par[2]
        
      }
      ## now determine the KLdiv value for the trial that was left out... 
      dataTrialWorker <- matrix(0, 1, 12)
      trialDataIndex <- idICases[workerTrialIndex]
      dataTrialWorker[,1] <- targetOC27[trialDataIndex]
      dataTrialWorker[,2] <- obj2OC27[trialDataIndex]
      dataTrialWorker[,3] <- obj3OC27[trialDataIndex]
      dataTrialWorker[,4] <- uttFeat[trialDataIndex]
      dataTrialWorker[,5] <- q1Feat[trialDataIndex]
      dataTrialWorker[,6] <- q2Feat[trialDataIndex]
      dataTrialWorker[,7:12] <- subjectResponses[trialDataIndex,1:6]
      if(numParams==1) {
        klDivCrossValValues[workerIndex, workerTrialIndex] <-
          choiceBasedModelKLDiv1param(abs(optRes1$minimum), dataTrialWorker)
      }else{ # 2 parameters
        klDivCrossValValues[workerIndex, workerTrialIndex] <-
          choiceBasedModelKLDiv2params(optRes2$par, dataTrialWorker)
      }
    } # end of loop moving through the worker trials
    
    # done with that worker
    workerIndex <- workerIndex + 1
  }
}

## 
## writing out tables
if(numParams==1) {
  write.csv(klDivWorkers, "X4_Data/x4CrossVal_KLDivOpt_SimpleModel_2019_0430.csv")
  write.csv(paramsWorkers, "X4_Data/x4CrossVal_Params_SimpleModel_2019_0430.csv")
  write.csv(klDivCrossValValues, "X4_Data/x4CrossVal_KLDivTrial_SimpleModel_2019_0430.csv")
}else{
  write.csv(klDivWorkers, "X4_Data/x4CrossVal_KLDivOpt_SimpleModel2p_2019_0430.csv")
  write.csv(paramsWorkers, "X4_Data/x4CrossVal_Params_SimpleModel2p_2019_0430.csv")
  write.csv(klDivCrossValValues, "X4_Data/x4CrossVal_KLDivTrial_SimpleModel2p_2019_0430.csv")
  
}


