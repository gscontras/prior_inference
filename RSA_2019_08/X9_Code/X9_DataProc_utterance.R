source("CommonCode/SRSA_StratUtt.R")
source("CommonCode/SRSA_UttChoiceOptimization_iterative.R")

x9data = read.csv(
  "X9_Data/ella_total_allDataCleaned.csv",
  header = TRUE,
  na.strings = c("", " ", "NA")
)

# adding feature property codes (which feature was uttereed, which features were questioned)
uttFeat <- ifelse(x9data$utterance=="green" | x9data$utterance=="red" | x9data$utterance=="blue", 3,
                  ifelse(x9data$utterance=="solid" | x9data$utterance=="striped" | x9data$utterance=="polka-dotted", 2, 1))
x9data$uttFeat <- uttFeat
targetFeat <- x9data$targetFeatureNum
utterance <- x9data$utterance

## adding the 1-27 target and object1, object2 & object3 code.
temp <- x9data$simulatedAnswer
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
targetOC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x9data$targetOC27 <- targetOC27

temp <- x9data$obj1
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj1OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x9data$obj1OC27 <- obj1OC27

temp <- x9data$obj2
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj2OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x9data$obj2OC27 <- obj2OC27

temp <- x9data$obj3
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj3OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x9data$obj3OC27 <- obj3OC27

## now determining the recorded subject responses 
subjectResponses <- matrix(0,length(x9data$X),3)

for(i in c(1:length(x9data$X))) {
  subjectResponses[i,1] <- x9data$normResponse0[i] + 1e-100
  subjectResponses[i,2] <- x9data$normResponse1[i] + 1e-100
  subjectResponses[i,3] <- x9data$normResponse2[i] + 1e-100
  #  subjectResponses[i,1:3] <- subjectResponses[i,1:3] / sum(subjectResponses[i,1:3]) # Ella already normalized the data
}

## ordering the recorded subject responses such that they can be compared directly 
#   to the model predictions 
##             (particularly for visual comparison in the table) 
subjectResponsesOrdered <- matrix(NA ,length(x9data$X),9)
for(i in c(1:length(x9data$X))) {
  for(j in 1:3) {  
    subjectResponsesOrdered[i, (j+(targetFeat[i]-1)*3)] <- subjectResponses[i,j]
  }
}
subjectResponsesOrdered <- round(subjectResponsesOrdered, digits=5)

## Reordering objects in input data

targetObject <- rep(NA, length(x9data$X))
object2 <- rep(NA, length(x9data$X))
object3 <- rep(NA, length(x9data$X))

for (i in 1:length(x9data$X)){
  if(targetOC27[i] == obj1OC27[i]){
    targetObject[i] <- targetOC27[i]
    object2[i] <- obj2OC27[i]
    object3[i] <- obj3OC27[i]
  } else if (targetOC27[i] == obj2OC27[i]) 
  {targetObject[i] <- obj2OC27[i]
  object2[i] <- obj1OC27[i]
  object3[i] <- obj3OC27[i]
  } else {
    targetObject[i] <- obj3OC27[i]
    object2[i] <- obj1OC27[i]
    object3[i] <- obj2OC27[i]
  }
}  

##### This part will change since there is only one utterance chosen #########

## now identify the first column number of the turker sliders and response pairs
sliderIndex <- grep("^pref1", colnames(x9pilotData))
## and use that index to determine all slider identities and corresponding slider values.
sliderUtteranceTypes <- matrix(NA, nrow(x9pilotData), 9)
sliderSetValues <- matrix(NA,  nrow(x9pilotData), 9)
for(i in c(1:9)) {
  colIndex <- sliderIndex + (i-1) * 2
  relRows <- which(!is.na(x9pilotData[[colIndex]]))
  for(j in c(1:length(relRows) ) ) { 
    sliderUtteranceTypes[relRows[j], i] <- which(allUtterancesNew1==x9pilotData[[colIndex]][relRows[j]])
    sliderSetValues[relRows[j], i] <- x9pilotData[[colIndex+1]][relRows[j]]
  }
}
### normalizing the turker estimates and setting them into the corresponding matrix.
bInfGainUttTurkers <- matrix(NA, nrow(x9pilotData), 9)
for(i in c(1:nrow(x9pilotData)) ) {
  s <- sum(sliderSetValues[i,c(1:x9pilotData$numFeatures[i])])
  if(s > 0) {
    sliderSetValues[i,c(1:x9pilotData$numFeatures[i])] <- sliderSetValues[i,c(1:x9pilotData$numFeatures[i])] / s
  }else{
    sliderSetValues[i,c(1:x9pilotData$numFeatures[i])] <- 1 / (x9pilotData$numFeatures[i])
  }
  bInfGainUttTurkers[i, sliderUtteranceTypes[i,c(1:(x9pilotData$numFeatures[i]) )] ] <- sliderSetValues[i,c(1:(x9pilotData$numFeatures[i]) )]
  for(j in c(1:x9pilotData$numFeatures[i])) {
    if(is.na(sliderSetValues[i,j])) {
      print("ERRor")
    }
  }
}

############ Set up the parameters and KL values matrix ##########

## recording KL divergence and parameters (base model, 1 param, 2 params)
workerIDs <- x9pilotData$workerid
idMax <- max(workerIDs)
klDivUttWorkers <- matrix(0,length(unique(workerIDs)), 8)
paramsUttWorkers <- matrix(0,length(unique(workerIDs)), 11)

#######################################################
## Starting with simple base model determination:    ##
#######################################################
workerIndex <- 1
for(workerID in c(0:idMax)) {
  idICases <- which(workerIDs == workerID)
  if(length(idICases)>0) {
    klDivUttWorkers[workerIndex,1] <- workerID
    paramsUttWorkers[workerIndex,1] <- workerID
    ## based model -> no change in preferences!
    klDivUttWorkers[workerIndex,2] <- 0
    for(i in c(1:length(idICases))) {
      len <- x9pilotData$numFeatures[idICases[i]]
      for(j in c(1:len) ) {
        klDivUttWorkers[workerIndex, 2] <- klDivUttWorkers[workerIndex, 2] + 
          sliderSetValues[idICases[i],j] * 
          (log(sliderSetValues[idICases[i],j] + 1e-100) - log(1/len))
        if(is.na(klDivUttWorkers[workerIndex, 2])) {
          print("Is NA!???")
          print(c(sliderSetValues[idICases[i],j], log(1/len), i, j, len))
          j <- 10
          i <- length(idICases) +1 
        }
      }
    }
    ## done with this worker -> proceed
    workerIndex <- workerIndex + 1
  }
}

############ Set up the data matrix ##########

#######################
## Optimizing (i.e. minimzing) the KL Divergence values for each worker...
## starting with 1 parameter RSA model optimizations... 
# data is a matrix with data rows. column structure: [1:OC1,OC2,OC3,4:numUttOptions,7-X(max 15):TurkerSliderValues]
workerIndex <- 1
for(workerID in c(0:idMax)) {
  idICases <- which(workerIDs == workerID)
  if(length(idICases)>0) {
    ## generating data matrix for the purpose of optimization
    dataWorker <- matrix(0, length(idICases), 8)
    dataWorker[,1] <- targetObject[idICases]
    dataWorker[,2] <- object2[idICases]
    dataWorker[,3] <- object3[idICases]
    dataWorker[,4] <- uttFeat[idICases]
    dataWorker[,5] <- targetFeat[idICases]
    dataWorker[,6] <- utterance[idICases]
    
    # print(dataWorker)
    # now optimize for one parameter... 
    optRes1 <- optimize(SimpleRSAModelUttKLDivParamA, c(0,1e+10), dataWorker)
    optRes2 <- optimize(SimpleRSAModelUttKLDivParamB, c(0,1e+10), dataWorker)   
    optRes3 <- optimize(SimpleRSAModelUttKLDivParamK, c(-10,10), dataWorker)   
    
    ## 1 param RSA Utt model
    klDivUttWorkers[workerIndex,3] <- optRes1$objective
    klDivUttWorkers[workerIndex,4] <- optRes2$objective
    klDivUttWorkers[workerIndex,5] <- optRes3$objective
    ## resulting parameter choice
    paramsUttWorkers[workerIndex,2] <- optRes1$minimum
    paramsUttWorkers[workerIndex,3] <- optRes2$minimum
    paramsUttWorkers[workerIndex,4] <- optRes3$minimum
    ####
    optRes2n1 <- optim(c(.2, .2), SimpleRSAModelUttKLDivParamBK, method="L-BFGS-B", gr=NULL, dataWorker,
                       lower = c(0,-10), upper = c(1e+10,10))
    optRes2n2 <- optim(c(.2, .2), SimpleRSAModelUttKLDivParamAK, method="L-BFGS-B", gr=NULL, dataWorker,
                       lower = c(0,-10), upper = c(1e+10,10))
    optRes3 <- optim(c(.2, .2, 1), SimpleRSAModelUttKLDivParamABK, method="L-BFGS-B", gr=NULL, dataWorker,
                     lower = c(0,0,-10), upper = c(1e+10,1e+10,10))
    ## 2 and 3 param RSA model2
    ## max likelihood parameter choice
    klDivUttWorkers[workerIndex,6] <- optRes2n1$value
    klDivUttWorkers[workerIndex,7] <- optRes2n2$value
    klDivUttWorkers[workerIndex,8] <- optRes3$value
    ## max likelihood parameter choice
    paramsUttWorkers[workerIndex,5] <- optRes2n1$par[1]
    paramsUttWorkers[workerIndex,6] <- optRes2n1$par[2]
    paramsUttWorkers[workerIndex,7] <- optRes2n2$par[1]
    paramsUttWorkers[workerIndex,8] <- optRes2n2$par[2]
    paramsUttWorkers[workerIndex,9] <- optRes3$par[1]
    paramsUttWorkers[workerIndex,10] <- optRes3$par[2]
    paramsUttWorkers[workerIndex,11] <- optRes3$par[3]
    ##    
    print(c("Done with worker ",workerIndex," with worder ID ", workerID))
    print(c(klDivUttWorkers[workerIndex,], paramsUttWorkers[workerIndex,]))
    ####
    workerIndex <- workerIndex + 1
  }
}

write.csv(klDivUttWorkers, "X3_Data/x3KLDivs_simpleRSA_indOpt_2019_10_11.csv")
write.csv(paramsUttWorkers, "X3_Data/x3Params_simpleRSA_indOpt_2019_10_11.csv")
