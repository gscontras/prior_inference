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
#utterance <- x9data$utterance

utterance <- rep(NA, length(x9data$X))
pickedUtterance <- rep(NA, length(x9data$X))

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

# Which utterance the subjects picked

for(i in c(1:length(x9data$X))) {
  utterance[i] <- match(as.character(x9data$utterance[i]),allUtterancesNew1) ## polka-dotted!!!
  currentObjects <- c(targetObject[i],object2[i],object3[i]) 
  relevantUtterances <- determineValidUtterances(currentObjects)
  pickedUtterance[i] <- match(utterance[i], relevantUtterances)  
  featChoice <- uttFeat[i]
}

############ Set up the parameters and KL values matrix ##########

workerIDs <- x9data$workerid
idMax <- max(workerIDs)
logLikWorkers <- matrix(0,length(unique(workerIDs)), 8)
paramsUttWorkers <- matrix(0,length(unique(workerIDs)), 11)

#######################################################
## Starting with simple base model determination:    ##
#######################################################

workerIndex <- 1
for(workerID in c(0:idMax)) {
  idICases <- which(workerIDs == workerID)
  if(length(idICases)>0) {
    logLikWorkers[workerIndex,1] <- workerID
    logLikWorkers[workerIndex, 2] <- 0
    paramsUttWorkers[workerIndex,1] <- workerID
    
    dataWorker <- matrix(0, length(idICases), 6)
    dataWorker[,1] <- targetObject[idICases]
    dataWorker[,2] <- object2[idICases]
    dataWorker[,3] <- object3[idICases]
    dataWorker[,4] <- uttFeat[idICases]
    dataWorker[,5] <- targetFeat[idICases]
    dataWorker[,6] <- pickedUtterance[idICases] 
    
    ## base model -> no change in preferences!
    for(i in c(1:length(idICases))) { 
      #   logLikWorkers[workerIndex,2] <- 0
      currentObjects <- dataWorker[i,1:3] 
      relevantUtterances <- determineValidUtterances(currentObjects)
      irrelevantIndices <- which(relevantUtterances>(3*(dataWorker[i,5]-1)) & 
                                   relevantUtterances<(3*dataWorker[i,5] + 1))
      validUtterances <- relevantUtterances[-irrelevantIndices]  
      #    print(length(validUtterances))
      logLikWorkers[workerIndex, 2] <- logLikWorkers[workerIndex, 2] - log(1/length(validUtterances))  
      if(is.na(logLikWorkers[workerIndex, 2])) {
        print("Is NA!???")
      } 
    } 
    ## done with this worker -> proceed
    workerIndex <- workerIndex + 1
  }
}


## Optimizing (i.e. minimzing) the negative log likelihood values for each worker...
## starting with 1 parameter RSA model optimizations... 
## data is a matrix with data rows. column structure: 
## [1:tagrget object, 2: object 2, 3: object 3,4:uttered featue, 5: target feature,6: picked utterance]

logLikDefaultParam <- rep(NA, length(x9data$X))
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
    dataWorker[,6] <- pickedUtterance[idICases]
    #    logLikDefaultParam[workerIndex] <- SimpleRSAModelUttKLDiv_3params_iterative(dataWorker, 0,0,1) 
    # print(dataWorker)
    
    ## 1 param RSA Utt model optimizing for kl-value factor
    
    optRes3 <- optimize(SimpleRSAModelUttKLDivParamK_iterative, c(-10,10), dataWorker) 
    optRes4 <- optimize(SimpleRSAModelUttKLDivParamK_independent, c(-10,10), dataWorker)
    logLikWorkers[workerIndex,3] <- SimpleRSAModelUttKLDiv_3params_iterative(dataWorker, 0,0,1) 
    logLikWorkers[workerIndex,4] <- SimpleRSAModelUttKLDiv_3params_independent(dataWorker, 0,0,1) 
    logLikWorkers[workerIndex,5] <- optRes3$objective
    logLikWorkers[workerIndex,6] <- optRes4$objective
    logLikWorkers[workerIndex,7] <- optRes3$objective < optRes4$objective
    paramsUttWorkers[workerIndex,4] <- optRes3$minimum
    paramsUttWorkers[workerIndex,5] <- optRes4$minimum
    colnames(logLikWorkers) <- c("workerid","uniform","default_param_iter", "default_param_indep" ,
                                 "iterative", "independent", "iterative_better", "v8")
    print(c("Done with worker ",workerIndex," with worker ID ", workerID))
    print(c(logLikWorkers[workerIndex,], paramsUttWorkers[workerIndex,]))
    # ####
    workerIndex <- workerIndex + 1
  }
}

########### Additional optimization options #################

#   optRes1 <- optimize(SimpleRSAModelUttKLDivParamA, c(0,1e+10), dataWorker)
#   optRes2 <- optimize(SimpleRSAModelUttKLDivParamB, c(0,1e+10), dataWorker)   
#   logLikWorkers[workerIndex,3] <- optRes1$objective
#   logLikWorkers[workerIndex,4] <- optRes2$objective
#   paramsUttWorkers[workerIndex,2] <- optRes1$minimum
#   paramsUttWorkers[workerIndex,3] <- optRes2$minimum

#    optRes2n1 <- optim(c(.2, .2), SimpleRSAModelUttKLDivParamBK, method="L-BFGS-B", gr=NULL, dataWorker,
#                       lower = c(0,-10), upper = c(1e+10,10))
#    optRes2n2 <- optim(c(.2, .2), SimpleRSAModelUttKLDivParamAK, method="L-BFGS-B", gr=NULL, dataWorker,
#                       lower = c(0,-10), upper = c(1e+10,10))
#    optRes3 <- optim(c(.2, .2, 1), SimpleRSAModelUttKLDivParamABK, method="L-BFGS-B", gr=NULL, dataWorker,
#                     lower = c(0,0,-10), upper = c(1e+10,1e+10,10))

## 2 and 3 param RSA model2
## max likelihood parameter choice
#    logLikWorkers[workerIndex,6] <- optRes2n1$value
#    logLikWorkers[workerIndex,7] <- optRes2n2$value
#    logLikWorkers[workerIndex,8] <- optRes3$value
## max likelihood parameter choice
# paramsUttWorkers[workerIndex,5] <- optRes2n1$par[1]
# paramsUttWorkers[workerIndex,6] <- optRes2n1$par[2]
# paramsUttWorkers[workerIndex,7] <- optRes2n2$par[1]
# paramsUttWorkers[workerIndex,8] <- optRes2n2$par[2]
# paramsUttWorkers[workerIndex,9] <- optRes3$par[1]
# paramsUttWorkers[workerIndex,10] <- optRes3$par[2]
# paramsUttWorkers[workerIndex,11] <- optRes3$par[3]
##    

############### Recording output ##########################

write.csv(logLikWorkers, "X9_Data/x9_logLik_indOpt.csv")
write.csv(paramsUttWorkers, "X9_Data/x9Params_Lambda_indOpt.csv")
