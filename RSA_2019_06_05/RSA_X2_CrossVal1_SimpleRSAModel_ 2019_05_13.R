source("RSA_StratUttOptimization_SimpleRSA_2019_0507.R")
source("RSA_StratUttModelSimple_2019_0505.R")

# loading the raw pilot data (as Greg sent it on 2018/11)
x2pilotData<- read.csv("2-pilot.csv")

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
#   to the model predictions 
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
uniqueItemCodes <- sort(unique(x2pilotData$itemCode))

#######################
## Optimizing wrt KL divergence criterion
# parameter-based RSA model optimizations...
# leave one out parameter determination for each participant!

############
## Generating the Unique Item codes
constellationCode <- matrix(0,length(x2pilotData$X),6)
uniqueCCode <- rep(0, length(x2pilotData$X))
for(i in c(1:length(x2pilotData$X))) {
  objectConstellation <- c(targetOC27[i],obj2OC27[i],obj3OC27[i])
  featChoice <- uttFeat[i]
  constellationCode[i,] <- getConstellationCode(objectConstellation, featChoice)[[1]]
  uc <- 0
  for(j in c(1:6)) {
    uc <- (uc * 10) + constellationCode[i,j]
  }
  uniqueCCode[i] <- uc
}
x2pilotData$itemCode <- uniqueCCode

###################################################################################
parOptType <-3 ######## 1 or 2 params OR 1 and not obey.1 (= parOptType=3) number of parameters to be optimized. #####################
###################################################################################

llWorkers12 <- matrix(0,length(unique(workerIDs)), 15)
klDivCrossValValues <- matrix(0,length(unique(workerIDs)), 15)
if(parOptType==1) {
  paramsWorkers12 <- matrix(0,length(unique(workerIDs)), 15)
}else if(parOptType==2) {
  paramsWorkers12 <- matrix(0,length(unique(workerIDs)), 30)
}else if(parOptType==3) {
  paramsWorkers12 <- matrix(0,length(unique(workerIDs)), 15)
}

workerIndex <- 1
for(workerID in c(0:idMax)) {
  idICases <- which(workerIDs == workerID)
  if(length(idICases)>0) {
    print(c("Optimizing cross. val. w.r.t. worker with ID: ",workerID))
    workerItemCodes <- (x2pilotData$itemCode)[idICases]
    ## now moving through the trials and optimizing with respect to the other trials
    for(workerTrialIndex in c(1:length(idICases))) {
      trialItemCode <- workerItemCodes[workerTrialIndex]
      filteredTrialIndices <- which(workerItemCodes != trialItemCode)
      consideredIdICases <- idICases[filteredTrialIndices]
#      if(length(consideredIdICases) < 14) {
#        print(c("Multiple trials of same case in worker: ",workerID," with a length of ",
#                length(consideredIdICases)," of ",length(idICases)))
#      }
      ## generating data matrix containing the considered cases
      dataWorker <- matrix(0, length(consideredIdICases), 12)
      dataWorker[,1] <- targetOC27[consideredIdICases]
      dataWorker[,2] <- obj2OC27[consideredIdICases]
      dataWorker[,3] <- obj3OC27[consideredIdICases]
      dataWorker[,4] <- uttFeat[consideredIdICases]
      dataWorker[,5] <- q1Feat[consideredIdICases]
      dataWorker[,6] <- q2Feat[consideredIdICases]
      dataWorker[,7:12] <- subjectResponses[consideredIdICases,1:6]
    
      if(parOptType==1) {
        optRes1 <- optimize(RSAModelLL1_1simpleRSA, c(0,1e+10), dataWorker)
        llWorkers12[workerIndex,workerTrialIndex] <- optRes1$objective
        paramsWorkers12[workerIndex,workerTrialIndex] <- optRes1$minimum
      }else if(parOptType==2) {
        optRes2 <- optim(c(.2, .2), RSAModelLL2_simpleRSA, method="L-BFGS-B", gr=NULL, dataWorker,
                           lower = c(0,0), upper = c(1e+10,1e+10))
        llWorkers12[workerIndex,workerTrialIndex] <- optRes2$value
        paramsWorkers12[workerIndex,(workerTrialIndex-1)*2+1] <- optRes2$par[1]
        paramsWorkers12[workerIndex,(workerTrialIndex-1)*2+2] <- optRes2$par[2]
      }else if(parOptType==3) {
        optRes1 <- optimize(RSAModelLL1_1simpleRSA_notObey.1, c(0,1e+10), dataWorker)
        llWorkers12[workerIndex,workerTrialIndex] <- optRes1$objective
        paramsWorkers12[workerIndex,workerTrialIndex] <- optRes1$minimum
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

      if(parOptType==1) {
        klDivCrossValValues[workerIndex, workerTrialIndex] <-
          RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered_simpleRSA(dataTrialWorker, abs(optRes1$minimum), 0)
      }else if(parOptType==2) {
        klDivCrossValValues[workerIndex, workerTrialIndex] <- 
          RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered_simpleRSA(dataTrialWorker, optRes2$par[1], optRes2$par[2])
      }else if(parOptType==3) {
        klDivCrossValValues[workerIndex, workerTrialIndex] <-
          RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered_simpleRSA(dataTrialWorker, abs(optRes1$minimum), 0.1)
      }
      
    } # end of loop moving through the worker trials
    
    # done with that worker
    workerIndex <- workerIndex + 1
  }
}

## 
## writing out tables
if(parOptType==1) {
  write.csv(llWorkers12, "x2CrossVal_SimpleRSA_KLDivOpt_1parOpt_2019_0430.csv")
  write.csv(paramsWorkers12, "x2CrossVal_SimpleRSA_Params_1parOpt_2019_0430.csv")
  write.csv(klDivCrossValValues, "x2CrossVal_SimpleRSA_KLDivTrial_1parOpt_2019_0430.csv")
}else if(parOptType==2) {
  write.csv(llWorkers12, "x2CrossVal_SimpleRSA_KLDivOpt_12parOpt_2019_0430.csv")
  write.csv(paramsWorkers12, "x2CrossVal_SimpleRSA_Params_12parOpt_2019_0430.csv")
  write.csv(klDivCrossValValues, "x2CrossVal_SimpleRSA_KLDivTrial_12parOpt_2019_0430.csv")
}else if(parOptType==3) {
  write.csv(llWorkers12, "x2CrossVal_SimpleRSA_KLDivOpt_notObey.1_2019_0430.csv")
  write.csv(paramsWorkers12, "x2CrossVal_SimpleRSA_Params_notObey.1_2019_0430.csv")
  write.csv(klDivCrossValValues, "x2CrossVal_SimpleRSA_KLDivTrial_notObey.1_2019_0430.csv")
}



